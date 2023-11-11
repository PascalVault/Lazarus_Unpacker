unit shrink;

{$mode delphi}{$H+}

//Code by TurboPower Software
//Taken from library TurboPower Abbrevia
//License: MPL

interface

uses Classes;

const
  AbBufferSize = 32768;
  AbLastDisk = -1;
  AbLastImage = -1;

type
  PAbByteArray4K = ^TAbByteArray4K;
  TAbByteArray4K = array[1..4096] of Byte;
  PAbByteArray8K = ^TAbByteArray8K;
  TAbByteArray8K = array[0..8192] of Byte;
  PAbIntArray8K  = ^TAbIntArray8K;
  TAbIntArray8K  = array[0..8192] of SmallInt;

  PAbWordArray   = ^TAbWordArray;
  TAbWordArray   = array[0..65535 div SizeOf(Word)-1] of Word;
  PAbByteArray   = ^TAbByteArray;
  TAbByteArray   = array[0..65535-1] of Byte;
  PAbSmallIntArray = ^TAbSmallIntArray;
  TAbSmallIntArray = array[0..65535 div SizeOf(SmallInt)-1] of SmallInt;

  PAbIntegerArray = ^TAbIntegerArray;
  TAbIntegerArray = array[0..65535 div sizeof(integer)-1] of integer;

  TShrink = class
  private
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
    procedure uzUnShrink;
    procedure uzFlushOutBuf;
    procedure uzWriteByte(B : Byte);
    function uzReadBits(Bits : Byte) : Integer;
    procedure uzReadNextPrim;
  public
    constructor Create(InStream, OutStream: TStream);
    procedure Decode(UnpackedSize: Cardinal);
  end;

implementation

constructor TShrink.Create(InStream, OutStream: TStream);
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
end;

procedure TShrink.Decode(UnpackedSize: Cardinal);
begin
  FUnCompressedSize := UnpackedSize;

  FOutWriter := FOutStream;

  uzUnShrink;
  uzFlushOutBuf;
end;

procedure TShrink.uzReadNextPrim;
begin
  FInCnt := FZStream.Read( FInBuf, sizeof( FInBuf ) );
  FInEof := FInCnt = 0;
  {load first byte in buffer and set position counter}
  FCurByte := FInBuf[1];
  FInPos := 2;
end;

function TShrink.uzReadBits(Bits : Byte) : Integer;
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

procedure TShrink.uzFlushOutBuf;
  {-flushes the output buffer}
begin
  if (FOutPos <> 0) then begin
    FOutWriter.Write( FOutBuf^, FOutPos );
    Inc( FOutSent, FOutPos );
    FOutPos := 0;
  end;
end;

procedure TShrink.uzWriteByte(B : Byte);
  {-Write one byte to the output buffer}
begin
  FOutBuf^[FOutPos] := B;
  inc(FOutPos);
  if (FOutPos = AbBufferSize) or
     (Integer(FOutPos) + FOutSent = FUncompressedSize) then
    uzFlushOutBuf;
end;

procedure TShrink.uzUnShrink;
  {-Extract a file that was shrunk}
const
  MaxBits = 13;
  InitBits = 9;
  FirstFree = 257;
  Clear = 256;
  MaxCodeMax = 8192; {= 1 shl MaxBits}
  Unused = -1;
var
  CodeSize : SmallInt;
  NextFree : SmallInt;
  BaseChar : SmallInt;
  NewCode : SmallInt;
  OldCode : SmallInt;
  SaveCode : SmallInt;
  N, R : SmallInt;
  I : Integer;
  PrefixTable : PAbIntArray8K;      {used while processing shrunk files}
  SuffixTable : PAbByteArray8K;     {"}
  Stack : PAbByteArray8K;           {"}
  StackIndex : Integer;           {"}
begin
  CodeSize := InitBits;
{  MaxCode := (1 shl InitBits)-1;}
  NextFree := FirstFree;

  PrefixTable := nil;
  SuffixTable := nil;
  Stack := nil;

  try
    GetMem(PrefixTable, SizeOf(PrefixTable^));
    SuffixTable := AllocMem(SizeOf(SuffixTable^));
    GetMem(Stack, SizeOf(Stack^));

    FillChar(PrefixTable^, SizeOf(PrefixTable^), $FF);
    for NewCode := 255 downto 0 do begin
      PrefixTable^[NewCode] := 0;
      SuffixTable^[NewCode] := NewCode;
    end;

    OldCode := uzReadBits(CodeSize);
    if FInEof then
      Exit;
    BaseChar := OldCode;

    uzWriteByte(BaseChar);

    StackIndex := 0;
    while (not FInEof) do begin
      NewCode := uzReadBits(CodeSize);
      while (NewCode = Clear) and (not FInEof) do begin
        case uzReadBits(CodeSize) of
          1 : begin
                Inc(CodeSize);
              end;
          2 : begin
                {mark all nodes as potentially unused}
                for I := FirstFree to pred( NextFree ) do
                  PrefixTable^[I] := PrefixTable^[I] or Integer($8000);

                {unmark those used by other nodes}
                for N := FirstFree to NextFree-1 do begin
                  {reference to another node?}
                  R := PrefixTable^[N] and $7FFF;
                  {flag node as referenced}
                  if R >= FirstFree then
                    PrefixTable^[R] := PrefixTable^[R] and $7FFF;
                end;

                {clear the ones that are still marked}
                for I := FirstFree to pred( NextFree ) do
                  if PrefixTable^[I] < 0 then
                    PrefixTable^[I] := -1;

                {recalculate NextFree}
                NextFree := FirstFree;
                while (NextFree < MaxCodeMax) and
                      (PrefixTable^[NextFree] <> -1) do
                  Inc(NextFree);
              end;
        end;

        NewCode := uzReadBits(CodeSize);
      end;

      if FInEof then
        Exit;

      {save current code}
      SaveCode := NewCode;

      {special case}
      if PrefixTable^[NewCode] = Unused then begin
        Stack^[StackIndex] := BaseChar;
        Inc(StackIndex);
        NewCode := OldCode;
      end;

      {generate output characters in reverse order}
      while (NewCode >= FirstFree) do begin
        if PrefixTable^[NewCode] = Unused then begin
          Stack^[StackIndex] := BaseChar;
          Inc(StackIndex);
          NewCode := OldCode;
        end else begin
          Stack^[StackIndex] := SuffixTable^[NewCode];
          Inc(StackIndex);
          NewCode := PrefixTable^[NewCode];
        end;
      end;

      BaseChar := SuffixTable^[NewCode];
      uzWriteByte(BaseChar);

      {put them out in forward order}
      while (StackIndex > 0) do begin
        Dec(StackIndex);
        uzWriteByte(Stack^[StackIndex]);
      end;

      {add new entry to tables}
      NewCode := NextFree;
      if NewCode < MaxCodeMax then begin
        PrefixTable^[NewCode] := OldCode;
        SuffixTable^[NewCode] := BaseChar;
        while (NextFree < MaxCodeMax) and
              (PrefixTable^[NextFree] <> Unused) do
          Inc(NextFree);
      end;

      {remember previous code}
      OldCode := SaveCode;
    end;
  finally
    FreeMem(PrefixTable, SizeOf(PrefixTable^));
    FreeMem(SuffixTable, SizeOf(SuffixTable^));
    FreeMem(Stack, SizeOf(Stack^));
  end;
end;

end.
