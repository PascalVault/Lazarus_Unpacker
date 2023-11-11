unit implode;

{$mode delphi}{$H+}

//Code by TurboPower Software
//Taken from library TurboPower Abbrevia
//License: MPL

interface

uses Classes, AbSWStm;

const
  AbBufferSize = 32768;
  AbLastDisk = -1;
  AbLastImage = -1;

type
  PAbSfEntry = ^TAbSfEntry;
  TAbSfEntry =                       {entry in a Shannon-Fano tree}
  packed record
    case Byte of
      0 : (Code : Word; Value, BitLength : Byte);
      1 : (L : Integer);
  end;
  PAbSfTree = ^TAbSfTree;
  TAbSfTree =
  packed record                        {a Shannon-Fano tree}
    Entries : SmallInt;
    MaxLength : SmallInt;
    Entry : array[0..256] of TAbSfEntry;
  end;

  TAbZipDictionarySize = (dsInvalid, ds4K, ds8K);

const
  szLengthTree = SizeOf(TAbSfTree)-(192*SizeOf(TAbSfEntry));
  szDistanceTree = SizeOf(TAbSfTree)-(192*SizeOf(TAbSfEntry));
  szLitTree = SizeOf(TAbSfTree);

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

  TImplode = class
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

    FDictionarySize : TAbZipDictionarySize;
    FShannonFanoTreeCount : Byte;

    procedure AbReverseBits(var W : Word);
    procedure uzUnImplode;
    procedure uzFlushOutBuf;
    procedure uzWriteByte(B : Byte);
    function uzReadBits(Bits : Byte) : Integer;
    procedure uzReadNextPrim;
  public
    constructor Create(InStream, OutStream: TStream; ShannonCount, DictionarySize: Byte);
    procedure Decode(UnpackedSize: Cardinal);
  end;

implementation

constructor TImplode.Create(InStream, OutStream: TStream; ShannonCount, DictionarySize: Byte);
begin
  FBitsLeft := 0;
  FCurByte := 0;
  FInEof := False;
  FInCnt := 0;
  FOutSent := 0;
  FOutPos := 0;

  if ShannonCount = 2 then FShannonFanoTreeCount := 2
  else                     FShannonFanoTreeCount := 3;

  if DictionarySize = 4 then FDictionarySize := ds4K
  else                       FDictionarySize := ds8K;

  FOutBuf := AllocMem( AbBufferSize );
  FInPos := 1+SizeOf(FInBuf);

  FZStream := InStream;
  FOutStream := OutStream;
end;

procedure TImplode.Decode(UnpackedSize: Cardinal);
begin
  FUnCompressedSize := UnpackedSize;

  FOutWriter := TabSlidingWindowStream.Create(FOutStream)  ;

  uzUnImplode;
  uzFlushOutBuf;

  FOutWriter.Free;
end;

procedure TImplode.uzReadNextPrim;
begin
  FInCnt := FZStream.Read( FInBuf, sizeof( FInBuf ) );
  FInEof := FInCnt = 0;
  {load first byte in buffer and set position counter}
  FCurByte := FInBuf[1];
  FInPos := 2;
end;

function TImplode.uzReadBits(Bits : Byte) : Integer;
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

procedure TImplode.uzFlushOutBuf;
  {-flushes the output buffer}
begin
  if (FOutPos <> 0) then begin
    FOutWriter.Write( FOutBuf^, FOutPos );
    Inc( FOutSent, FOutPos );
    FOutPos := 0;
  end;
end;

procedure TImplode.uzWriteByte(B : Byte);
  {-Write one byte to the output buffer}
begin
  FOutBuf^[FOutPos] := B;
  inc(FOutPos);
  if (FOutPos = AbBufferSize) or
     (Integer(FOutPos) + FOutSent = FUncompressedSize) then
    uzFlushOutBuf;
end;


procedure TImplode.AbReverseBits(var W : Word);
  {-Reverse the order of the bits in W}
register;
const
  RevTable : array[0..255] of Byte = ($00, $80, $40, $C0, $20, $A0, $60,
   $E0, $10, $90, $50, $D0, $30, $B0, $70, $F0, $08, $88, $48, $C8, $28,
   $A8, $68, $E8, $18, $98, $58, $D8, $38, $B8, $78, $F8, $04, $84, $44,
   $C4, $24, $A4, $64, $E4, $14, $94, $54, $D4, $34, $B4, $74, $F4, $0C,
   $8C, $4C, $CC, $2C, $AC, $6C, $EC, $1C, $9C, $5C, $DC, $3C, $BC, $7C,
   $FC, $02, $82, $42, $C2, $22, $A2, $62, $E2, $12, $92, $52, $D2, $32,
   $B2, $72, $F2, $0A, $8A, $4A, $CA, $2A, $AA, $6A, $EA, $1A, $9A, $5A,
   $DA, $3A, $BA, $7A, $FA, $06, $86, $46, $C6, $26, $A6, $66, $E6, $16,
   $96, $56, $D6, $36, $B6, $76, $F6, $0E, $8E, $4E, $CE, $2E, $AE, $6E,
   $EE, $1E, $9E, $5E, $DE, $3E, $BE, $7E, $FE, $01, $81, $41, $C1, $21,
   $A1, $61, $E1, $11, $91, $51, $D1, $31, $B1, $71, $F1, $09, $89, $49,
   $C9, $29, $A9, $69, $E9, $19, $99, $59, $D9, $39, $B9, $79, $F9, $05,
   $85, $45, $C5, $25, $A5, $65, $E5, $15, $95, $55, $D5, $35, $B5, $75,
   $F5, $0D, $8D, $4D, $CD, $2D, $AD, $6D, $ED, $1D, $9D, $5D, $DD, $3D,
   $BD, $7D, $FD, $03, $83, $43, $C3, $23, $A3, $63, $E3, $13, $93, $53,
   $D3, $33, $B3, $73, $F3, $0B, $8B, $4B, $CB, $2B, $AB, $6B, $EB, $1B,
   $9B, $5B, $DB, $3B, $BB, $7B, $FB, $07, $87, $47, $C7, $27, $A7, $67,
   $E7, $17, $97, $57, $D7, $37, $B7, $77, $F7, $0F, $8F, $4F, $CF, $2F,
   $AF, $6F, $EF, $1F, $9F, $5F, $DF, $3F, $BF, $7F, $FF);
begin
  W := RevTable[Byte(W shr 8)] or Word(RevTable[Byte(W)] shl 8);
end;


procedure TImplode.uzUnImplode;
    {-Extract an imploded file}
const
  szLengthTree = SizeOf(TAbSfTree)-(192*SizeOf(TAbSfEntry));
  szDistanceTree = SizeOf(TAbSfTree)-(192*SizeOf(TAbSfEntry));
  szLitTree = SizeOf(TAbSfTree);
var
  Length : Integer;
  DIndex : Integer;
  Distance : Integer;
  SPos : Integer;
  MyByte : Byte;
  DictBits : Integer;             {number of bits used in sliding dictionary}
  MinMatchLength : Integer;       {minimum match length}
  LitTree : PAbSfTree;              {Literal tree}
  LengthTree : PAbSfTree;           {Length tree}
  DistanceTree : PAbSfTree;         {Distance tree}

  procedure uzLoadTree(var T; TreeSize : Integer);
    {-Load one Shannon-Fano tree}
  var
    I : Word;
    Tree : TAbSfTree absolute T;

    procedure GenerateTree;
      {-Generate a Shannon-Fano tree}
    var
      C : Word;
      CodeIncrement : Integer;
      LastBitLength : Integer;
      I : Integer;
    begin
      C := 0;
      CodeIncrement := 0;
      LastBitLength := 0;

      for I := Tree.Entries-1 downto 0 do
        with Tree.Entry[I] do begin
          Inc(C, CodeIncrement);
          if BitLength <> LastBitLength then begin
            LastBitLength := BitLength;
            CodeIncrement := 1 shl (16-LastBitLength);
          end;
          Code := C;
        end;
    end;

    procedure SortLengths;
      {-Sort the bit lengths in ascending order, while retaining the order
        of the original lengths stored in the file}
    var
      XL : Integer;
      XGL : Integer;
      TXP  : PAbSfEntry;
      TXGP : PAbSfEntry;
      X, Gap : Integer;
      Done : Boolean;
      LT : Integer;
    begin
      Gap := Tree.Entries shr 1;
      repeat
        repeat
          Done := True;
          for X := 0 to (Tree.Entries-1)-Gap do begin
            TXP := @Tree.Entry[X];
            TXGP := @Tree.Entry[X+Gap];
            XL := TXP^.BitLength;
            XGL := TXGP^.BitLength;
            if (XL > XGL) or
               ((XL = XGL) and (TXP^.Value > TXGP^.Value)) then begin
              LT := TXP^.L;
              TXP^.L := TXGP^.L;
              TXGP^.L := LT;
              Done := False;
            end;
          end;
        until Done;

        Gap := Gap shr 1;
      until (Gap = 0);
    end;

    procedure uzReadLengths;
      {-Read bit lengths for a tree}
    var
      TreeBytes : Integer;
      I, J, K : Integer;
      Num, Len : Integer;
      B : Byte;
    begin
      {get number of bytes in compressed tree}
      TreeBytes := uzReadBits(8)+1;

      I := 0;
      Tree.MaxLength := 0;

      {High nibble: Number of values at this bit length + 1.
       Low  nibble: Bits needed to represent value + 1}
      for J := 1 to TreeBytes do begin
        B := uzReadBits(8);
        Len := (B and $0F)+1;
        Num := (B shr 4)+1;

        for K := I to I+Num-1 do
          with Tree, Entry[K] do begin
            if Len > MaxLength then
              MaxLength := Len;
            BitLength := Len;
            Value := K;
          end;
        Inc(I, Num);
      end;
    end;

  begin
    Tree.Entries := TreeSize;
    uzReadLengths;
    SortLengths;
    GenerateTree;
    for I := 0 to TreeSize-1 do
      AbReverseBits(Tree.Entry[I].Code);
  end;

  function uzReadTree(var T) : Byte;
    {-Read next byte using a Shannon-Fano tree}
  var
    Bits : Integer;
    CV   : Word;
    E    : Integer;
    Cur  : Integer;
  var
    Tree : TAbSfTree absolute T;
  begin
    Result := 0;
    Bits := 0;
    CV := 0;
    Cur := 0;
    E := Tree.Entries;
    repeat
      CV := CV or (uzReadBits(1) shl Bits);
      Inc(Bits);
      while Tree.Entry[Cur].BitLength < Bits do begin
        Inc(Cur);
        if Cur >= E then
          Exit;
      end;
      while Tree.Entry[Cur].BitLength = Bits do begin
        if Tree.Entry[Cur].Code = CV then begin
          Result := Tree.Entry[Cur].Value;
          Exit;
        end;
        Inc(Cur);
        if Cur >= E then
          Exit;
      end;
    until False;
  end;

begin
  {do we have an 8K dictionary?}
  if FDictionarySize = ds8K then
    DictBits := 7
  else
    DictBits := 6;

  {allocate trees}
  LengthTree := AllocMem(szLengthTree);
  DistanceTree := AllocMem(szDistanceTree);
  LitTree := nil;
  try
    {do we have a Literal tree?}
    MinMatchLength := FShannonFanoTreeCount;
    if MinMatchLength = 3 then begin
      LitTree := AllocMem(szLitTree);
      uzLoadTree(LitTree^, 256);
    end;

    {load the other two trees}
    uzLoadTree(LengthTree^, 64);
    uzLoadTree(DistanceTree^, 64);

    while (not FInEof) and (FOutSent + Integer(FOutPos) < FUncompressedSize) do
      {is data literal?}
      if Boolean(uzReadBits(1)) then begin
        {if MinMatchLength = 3 then we have a Literal tree}
        if (MinMatchLength = 3) then
          uzWriteByte( uzReadTree(LitTree^) )
        else
          uzWriteByte( uzReadBits(8) );
      end
      else begin
        {data is a sliding dictionary}
        Distance := uzReadBits(DictBits);

        {using the Distance Shannon-Fano tree, read and decode the
         upper 6 bits of the Distance value}
        Distance := Distance or (uzReadTree(DistanceTree^) shl DictBits);

        {using the Length Shannon-Fano tree, read and decode the Length value}
        Length := uzReadTree(LengthTree^);
        if Length = 63 then
          Inc(Length, uzReadBits(8));
        Inc(Length, MinMatchLength);

        {move backwards Distance+1 bytes in the output stream, and copy
         Length characters from this position to the output stream.
         (if this position is before the start of the output stream,
         then assume that all the data before the start of the output
         stream is filled with zeros)}
        DIndex := (FOutSent + Integer(FOutPos))-(Distance+1);
        while Length > 0 do begin
          if DIndex < 0 then
            uzWriteByte(0)
          else begin
            uzFlushOutBuf;
            SPos := FOutWriter.Position;
            FOutWriter.Position := DIndex;
            FOutWriter.Read( MyByte, 1 );
            FOutWriter.Position := SPos;
            uzWriteByte(MyByte);
          end;
          Inc(DIndex);
          Dec(Length);
        end;
      end;
  finally
    if (LitTree <> nil) then
      FreeMem(LitTree, szLitTree);
    FreeMem(LengthTree, szLengthTree);
    FreeMem(DistanceTree, szDistanceTree);
  end;
end;

end.
