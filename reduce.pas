unit reduce;

{$mode delphi}{$H+}

//Code by TurboPower Software
//Taken from library TurboPower Abbrevia
//License: MPL

interface

uses
  Classes, SysUtils, AbSWStm;

const
  AbBufferSize = 32768;
  AbLastDisk = -1;
  AbLastImage = -1;

type
  TAbFollower =                    {used to expand reduced files}
    packed record
      Size : Byte;                {size of follower set}
      FSet : array[0..31] of Byte; {follower set}
    end;
  PAbFollowerSets = ^TAbFollowerSets;
  TAbFollowerSets = array[0..255] of TAbFollower;
  PAbByteArray4K = ^TAbByteArray4K;
  TAbByteArray4K = array[1..4096] of Byte;
  PAbByteArray   = ^TAbByteArray;
  TAbByteArray   = array[0..65535-1] of Byte;

  { TReduce }

  TReduce = class
  private
    FReduction: Byte;
    FBitsLeft: Byte;
    FInPos : Integer;                {current position in input buffer}
    FInCnt : Integer;                {number of bytes in input buffer}
    FCurByte : Byte;                 {current input byte}
    FInBuf : TAbByteArray4K;
    FInEof  : Boolean;               {set when stream read returns 0}
    FZStream : TStream;
    FOutBuf : PAbByteArray;          {output buffer}
    FOutSent : Integer;              {number of bytes sent to output buffer}
    FOutPos : Cardinal;              {current position in output buffer}
    FBitSValid : Byte;               {Number of valid bits}
    FUnCompressedSize : Integer;
    FOutWriter : TStream;
    FOutStream: TStream;
    procedure uzUnReduce;
    procedure uzFlushOutBuf;
    procedure uzWriteByte(B : Byte);
    function uzReadBits(Bits : Byte) : Integer;
    procedure uzReadNextPrim;
  public
    constructor Create(InStream, OutStream: TStream; Reduction: Byte);
    procedure Decode(UnpackedSize: Cardinal);
  end;

implementation

constructor TReduce.Create(InStream, OutStream: TStream; Reduction: Byte);
begin
  FBitsLeft := 0;
  FCurByte := 0;
  FInEof := False;
  FInCnt := 0;
  FOutSent := 0;
  FOutPos := 0;

  FOutBuf := AllocMem( AbBufferSize );
  FInPos := 1+SizeOf(FInBuf);

  FZStream := InStream;
  FOutStream := OutStream;

  if Reduction < 1 then Reduction := 1
  else if Reduction > 4 then Reduction := 4;

  FReduction := Reduction;
end;

procedure TReduce.Decode(UnpackedSize: Cardinal);
begin
  FUnCompressedSize := UnpackedSize;

  FOutWriter := TabSlidingWindowStream.Create(FOutStream);

  uzUnReduce;
  uzFlushOutBuf;

  FOutWriter.Free;
end;

procedure TReduce.uzReadNextPrim;
begin
  FInCnt := FZStream.Read( FInBuf, sizeof( FInBuf ) );
  FInEof := FInCnt = 0;
  {load first byte in buffer and set position counter}
  FCurByte := FInBuf[1];
  FInPos := 2;
end;

function TReduce.uzReadBits(Bits : Byte) : Integer;
  {-Read the specified number of bits}
var
  SaveCurByte, Delta, SaveBitsLeft : Byte;
begin
  {read next byte if we're out of bits}
  if FBitsLeft = 0 then begin
    {do we still have a byte buffered?}
    if FInPos <= FInCnt then begin
      {get next byte out of buffer and advance position counter}
      FCurByte := FInBuf[FInPos];
      Inc(FInPos);
    end
    {are there any left to read?}
    else
      uzReadNextPrim;

    FBitsLeft := 8;
  end;
  if ( Bits < FBitsLeft ) then begin
    Dec( FBitsLeft, Bits );
    Result := ((1 shl Bits) - 1) and FCurByte;
    FCurByte := FCurByte shr Bits;
  end
  else if ( Bits = FBitsLeft ) then begin
    Result := FCurByte;
    FCurByte := 0;
    FBitsLeft := 0;
  end
  else begin
    SaveCurByte := FCurByte;
    SaveBitsLeft := FBitsLeft;
    {number of additional bits that we need}
    Delta := Bits - FBitsLeft;
    {do we still have a byte buffered?}
    if FInPos <= FInCnt then begin
      {get next byte out of buffer and advance position counter}
      FCurByte := FInBuf[FInPos];
      Inc(FInPos);
    end
    {are there any left to read?}
    else
      uzReadNextPrim;

    FBitsLeft := 8;
    Result := ( uzReadBits( Delta ) shl SaveBitsLeft ) or SaveCurByte;
  end;
end;

procedure TReduce.uzFlushOutBuf;
  {-flushes the output buffer}
begin
  if (FOutPos <> 0) then begin
    FOutWriter.Write( FOutBuf^, FOutPos );
    Inc( FOutSent, FOutPos );
    FOutPos := 0;
  end;
end;

procedure TReduce.uzWriteByte(B : Byte);
  {-Write one byte to the output buffer}
begin
  FOutBuf^[FOutPos] := B;
  inc(FOutPos);
  if (FOutPos = AbBufferSize) or
     (Integer(FOutPos) + FOutSent = FUncompressedSize) then
    uzFlushOutBuf;
end;

procedure TReduce.uzUnReduce;
const
  FactorMasks : array[1..4] of Byte = ($7F, $3F, $1F, $0F);
  DLE = 144;
var
  C, Last : Byte;
  OpI : LongInt;
  I, J, Sz : Integer;
  D : Word;
  SPos : LongInt;
  MyByte : Byte;
  Factor : Byte;                  {reduction Factor}
  FactorMask : Byte;              {bit mask to use based on Factor}
  Followers : PAbFollowerSets;      {array of follower sets}
  State : Integer;                {used while processing reduced files}
  V : Integer;                    {"}
  Len : Integer;                  {"}


  function BitsNeeded( i : Byte ) : Word;
  begin
    dec( i );
    Result := 0;
    repeat
      inc( Result );
      i := i shr 1;
    until i = 0;
  end;

begin

  GetMem(Followers, SizeOf(TAbFollowerSets));
  try
    Factor := FReduction;//Ord( FCompressionMethod ) - 1;
    FactorMask := FactorMasks[Factor];
    State := 0;
    C := 0;
    V := 0;
    Len := 0;
    D := 0;

    {load follower sets}
    for I := 255 downto 0 do begin
      Sz := uzReadBits(6);
      Followers^[I].Size := Sz;
      Dec(Sz);
      for J := 0 to Sz do
        Followers^[I].FSet[J] := uzReadBits(8);
    end;

    while (not FInEof) and ((FOutSent + LongInt(FOutPos)) < FUncompressedSize) do begin
      Last := C;
      with Followers^[Last] do
        if Size = 0 then
          C := uzReadBits(8)
        else begin
          C := uzReadBits(1);
          if C <> 0 then
            C := uzReadBits(8)
          else
            C := FSet[uzReadBits(BitsNeeded(Size))];
        end;

      if FInEof then
        Exit;

      case State of
        0 :
          if C <> DLE then
            uzWriteByte(C)
          else
            State := 1;
        1 :
          if C <> 0 then begin
            V := C;
            Len := V and FactorMask;
            if Len = FactorMask then
              State := 2
            else
              State := 3;
          end
          else begin
            uzWriteByte(DLE);
            State := 0;
          end;

        2 :
          begin
            Inc(Len, C);
            State := 3;
          end;

        3 :
          begin
            case Factor of
              1 : D := (V shr 7) and $01;
              2 : D := (V shr 6) and $03;
              3 : D := (V shr 5) and $07;
              4 : D := (V shr 4) and $0f;
            else
              raise Exception.Create('Wrong factor');
            end;
            {Delphi raises compiler Hints here, saying D might
             be undefined...  If Factor is not in [1..4], the
             exception gets raised, and we never execute the following
             line}
            OpI := (FOutSent + LongInt(FOutPos))-(Swap(D)+C+1);

            for I := 0 to Len+2 do begin
              if OpI < 0 then
                uzWriteByte(0)
              else if OpI >= FOutSent then
                uzWriteByte(FOutBuf[OpI - FOutSent])
              else begin
                SPos := FOutWriter.Position;
                FOutWriter.Position := OpI;
                FOutWriter.Read( MyByte, 1 );
                FOutWriter.Position := SPos;
                uzWriteByte(MyByte);
              end;
              Inc(OpI);
            end;

            State := 0;
          end;
      end;
    end;
  finally
    FreeMem(Followers, SizeOf(Followers^));
  end;
end;


end.

