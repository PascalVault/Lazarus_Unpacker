unit PV_Unpack;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-11-13

interface

uses
  Classes, SysUtils, DateUtils, Math, HasherBase, ZipCrypto, Implode, Shrink, Reduce, dcl_implode, RLE90, LZW,
  ArcMethod,
  Squeeze, ZStream, bzip2stream, ULZMADecoder, LzhHuff, LHA4, DLTUnpack, TxtEncodings, Dialogs;

type
  TPackMethod = (pmStore, pmDeflate, pmImplode2, pmImplode3, pmShrink, pmReduce1, pmReduce2, pmReduce3, pmReduce4,
  pmDCLImplode, pmBzip2, pmLh1, pmLh4, pmLh5, pmLh6, pmLh7, pmLhX, pmRLE90, pmSqueeze, pmCrunch8, pmSquash, pmSquashLZW,
  pmLzma, pmT64, pmRff, pmUUE, pmXXE, pmB64, pmYenc, pmDLT, pmOther);

  TEncryption = (enNone, enZipCrypto);

  TOpResult = (orFail, orOK, orVerified);

  TFile = record
    Name: String;
    PackedSize: Cardinal;
    UnpackedSize: Cardinal;
    ModDate: TDateTime;
    CRC32: Cardinal;
    Offset: Cardinal;
    PackMethod: TPackMethod;
    Encryption: TEncryption;
    Extra: Cardinal;
  end;

  { TUnpack }

  TUnpack = class
  private
  public
    FHasherClass: THasherClass;
    FFiles: array of TFile;
    FStream: TStream;
    FCount: Integer;
    FSize: Integer;
    procedure AddFile(AFile: TFile);
    procedure Progress(Position, Size: Int64);
  public
    property Count: Integer read FCount;
    function GetName(Index: Integer): String;
    function GetSize(Index: Integer): Int64;
    function GetCRC(Index: Integer): Cardinal;
    function GetPackedSize(Index: Integer): Int64;
    function GetDateTime(Index: Integer): TDateTime;
    function CanUnpack(Index: Integer): Boolean;
    function Extract(Index: Integer; Str: TStream): TOpResult;
    constructor Create(Str: TStream); virtual;
    destructor Destroy;
  end;

  { TProgressStream }

  TProgressStream = class(TStream)
  private
    FStream: TStream;
    FTotalSize: Int64;
    FHasher: THasherBase;
    FTotalWritten: Int64;
  public
    constructor Create(Str: TStream; TotalSize: Int64; Hasher: THasherBase);
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  function ReadStrNull(Str: TStream): String;
  function Dos2DateTime(Time, Date: Word): TDateTime;
  function Unix2DateTime(DateTime: Cardinal): TDateTime;
  function Win2DateTime(DateTime: Int64): TDateTime;
  function Unk2DateTime(DateTime: Cardinal): TDateTime;
  function Amiga2DateTime(DateTime: Cardinal): TDateTime;

implementation

function Unix2DateTime(DateTime: Cardinal): TDateTime;
begin
  Result := UnixToDateTime(DateTime);
end;

function Win2DateTime(DateTime: Int64): TDateTime;
const OA_ZERO_TICKS = UInt64(94353120000000000);
      TICKS_PER_DAY = UInt64(864000000000);
begin
  //100-nanosecond intervals since January 1, 1601
  Result := (Real(DateTime) - OA_ZERO_TICKS) / TICKS_PER_DAY;
end;

function Unk2DateTime(DateTime: Cardinal): TDateTime;
begin
  Result := DosDateTimeToDateTime(DateTime); //??
end;

function Amiga2DateTime(DateTime: Cardinal): TDateTime;
//not sure if that's "amiga" but used in LZX amiga format
var D,M,Y,HH,MM,SS: Integer;
begin
  DateTime := SwapEndian(DateTime);

  Y  := (DateTime shr 17) and $3F;
  M  := (DateTime shr 23) and $0F;
  D  := (DateTime shr 27) and $1F;
  Y  := Y + 1970;
  M  := M + 1;

  HH := (DateTime shr 12) and $1F;
  MM := (DateTime shr  6) and $3F;
  SS := (DateTime       ) and $3F;

  Result := EncodeDateTime(Y,M,D,HH,MM,SS,0);
end;

function Dos2DateTime(Time, Date: Word): TDateTime;
begin
  if (Date = 0) or (Time = 0) then Exit(0);
  Result := DosDateTimeToDateTime((Date shl 16) + Time);
end;

function ReadStrNull(Str: TStream): String;
var TempLen, Poss: Integer;
    Temp: String;
begin
  SetLength(Temp, 500);
  TempLen := Str.Read(Temp[1], 500);
  Poss := Pos(#0, Temp);
  Result := Copy(Temp, 1, Poss-1);
  Str.Position := Str.Position - TempLen + Poss;
end;

{ TProgressStream }

constructor TProgressStream.Create(Str: TStream; TotalSize: Int64;
  Hasher: THasherBase);
begin
  inherited Create;
  FStream := Str;
  FHasher := Hasher;
  FTotalSize := TotalSize;
  FTotalWritten := 0;
end;

function TProgressStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FStream.Write(Buffer, Count);

  if FHasher <> nil then
  FHasher.Update(@Buffer, Count);

  Inc(FTotalWritten, Count);
end;

function TProgressStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result:= FStream.Seek(Offset, Origin);
end;


procedure TUnpack.AddFile(AFile: TFile);
begin
  FFiles[FCount] := AFile;
  Inc(FCount);

  if FCount = FSize then begin
    Inc(FSize, 1000);
    SetLength(FFiles, FSize);
  end;
end;

procedure TUnpack.Progress(Position, Size: Int64);
begin
  //
end;

function TUnpack.GetName(Index: Integer): String;
begin
  Result := FFiles[Index].Name;
end;

function TUnpack.GetSize(Index: Integer): Int64;
begin
  Result := FFiles[Index].UnpackedSize;
end;

function TUnpack.GetCRC(Index: Integer): Cardinal;
begin
  Result := FFiles[Index].CRC32;
end;

function TUnpack.GetPackedSize(Index: Integer): Int64;
begin
  Result := FFiles[Index].PackedSize;
end;

function TUnpack.GetDateTime(Index: Integer): TDateTime;
begin
  Result := FFiles[Index].ModDate;
end;

function TUnpack.CanUnpack(Index: Integer): Boolean;
begin
  Result := FFiles[Index].PackMethod <> pmOther;
end;

function TUnpack.Extract(Index: Integer; Str: TStream): TOpResult;
var Dec: TDecompressionStream;
    Dec2: TDecompressBzip2Stream;
    ReadLen: Integer;
    Buf: array of Byte;
    Lzma: TLZMADecoder;
    Prop: array[0..4] of Byte;
    Temp: Word;
    i: Integer;
    Lzh1: TLZH;
    Lzh4: TLHA_Base;
    Hasher: THasherBase;
    ArchiveCRC, CalcCRC: String;
    ProgStr: TProgressStream;
    Crypto: TZipCrypto;
    Implode: TImplode;
    Shrink: TShrink;
    Reduce: TReduce;
    DCL: TDCL_Implode;
    RLE90: TRLE90;
    SQ: TSqueeze;
    Sqs: TSquash;
    Cru: TCrunch8;
    LZW: TLZW;
begin
  try
    Result := orOK;

    Hasher := nil;
    if FHasherClass <> nil then
    Hasher := FHasherClass.Create;

 //   if FFiles[Index].Encryption = enNone then begin
      ProgStr := TProgressStream.Create(Str, FFiles[Index].UnpackedSize, Hasher);
 {   end
    else if FFiles[Index].Encryption = enZipCrypto then begin
      ProgStr := TProgressStream.Create(Crypto, FFiles[Index].UnpackedSize, Hasher);
      Crypto := TZipCrypto.Create(Str, CRC, FFiles[Index].UnpackedSize);
    end;
  }
    FStream.Position := FFiles[Index].Offset;

    if FFiles[Index].PackMethod = pmDeflate then begin
      Dec := TDecompressionStream.Create(FStream, True);

      ProgStr.CopyFrom(Dec, FFiles[Index].UnpackedSize);

      Dec.Free;
    end
    else if FFiles[Index].PackMethod = pmLh1 then begin

      Lzh1 := TLZH.Create(FStream, ProgStr);
      Lzh1.Decode(FFiles[Index].UnpackedSize);
      Lzh1.Free;

    end
    else if FFiles[Index].PackMethod in [pmImplode2, pmImplode3] then begin

      if FFiles[Index].PackMethod = pmImplode2 then
        Implode := TImplode.Create(FStream, ProgStr, 2, FFiles[Index].Extra)
      else
        Implode := TImplode.Create(FStream, ProgStr, 3, FFiles[Index].Extra);

      Implode.Decode(FFiles[Index].UnpackedSize);
      Implode.Free;

    end
    else if FFiles[Index].PackMethod = pmShrink then begin

      Shrink := TShrink.Create(FStream, ProgStr);
      Shrink.Decode(FFiles[Index].UnpackedSize);
      Shrink.Free;

    end
    else if FFiles[Index].PackMethod = pmDCLImplode then begin

      DCL := TDCL_Implode.Create(FStream, ProgStr);
      DCL.Decode(FFiles[Index].PackedSize, FFiles[Index].UnpackedSize);
      DCL.Free;

    end
    else if FFiles[Index].PackMethod in [pmReduce1, pmReduce2, pmReduce3, pmReduce4] then begin

      case FFiles[Index].PackMethod of
        pmReduce1 : Reduce := TReduce.Create(FStream, ProgStr, 1);
        pmReduce2 : Reduce := TReduce.Create(FStream, ProgStr, 2);
        pmReduce3 : Reduce := TReduce.Create(FStream, ProgStr, 3);
        pmReduce4 : Reduce := TReduce.Create(FStream, ProgStr, 4);
      end;

      Reduce.Decode(FFiles[Index].UnpackedSize);
      Reduce.Free;

    end
    else if FFiles[Index].PackMethod = pmRLE90 then begin

      RLE90 := TRLE90.Create(FStream, ProgStr);
      RLE90.Decode(FFiles[Index].PackedSize);
      RLE90.Free;

    end
    else if FFiles[Index].PackMethod = pmSqueeze then begin

      SQ := TSqueeze.Create(FStream, ProgStr);
      SQ.Decode(FFiles[Index].PackedSize);
      SQ.Free;

    end
    else if FFiles[Index].PackMethod = pmSquash then begin

      Sqs := TSquash.Create(FStream, ProgStr);
      Sqs.Decode(FFiles[Index].PackedSize);
      Sqs.Free;

    end
    else if FFiles[Index].PackMethod = pmSquashLZW then begin
      //this should be exactly the same as Squash but there must be a small difference in implementation somewhere,
      //some parameter or something must be different

      LZW := TLZW.Create(FStream, ProgStr);
      LZW.Decode(FFiles[Index].Extra, FFiles[Index].PackedSize);
      LZW.Free;

    end
    else if FFiles[Index].PackMethod = pmCrunch8 then begin

      Cru := TCrunch8.Create(FStream, ProgStr);
      Cru.Decode(FFiles[Index].PackedSize);
      Cru.Free;

    end
    else if FFiles[Index].PackMethod in [pmLh4, pmLh5, pmLh6, pmLh7, pmLhX] then begin

      case FFiles[Index].PackMethod of
        pmLh4 : Lzh4 := TLha4.Create(FStream, ProgStr);
        pmLh5 : Lzh4 := TLha5.Create(FStream, ProgStr);
        pmLh6 : Lzh4 := TLha6.Create(FStream, ProgStr);
        pmLh7 : Lzh4 := TLha7.Create(FStream, ProgStr);
        pmLhX : Lzh4 := TLhaX.Create(FStream, ProgStr);
      end;

      Lzh4.Decode(FFiles[Index].UnpackedSize);
      Lzh4.Free;

    end
    else if FFiles[Index].PackMethod = pmUUE then begin

      DecodeUUE(FStream, Str, FFiles[Index].PackedSize);

    end
    else if FFiles[Index].PackMethod = pmXXE then begin

      DecodeXXE(FStream, Str, FFiles[Index].PackedSize);

    end
    else if FFiles[Index].PackMethod = pmB64 then begin

      DecodeB64(FStream, Str, FFiles[Index].PackedSize);

    end
    else if FFiles[Index].PackMethod = pmYENC then begin

      DecodeYENC(FStream, Str, FFiles[Index].PackedSize);

    end
    else if FFiles[Index].PackMethod = pmDLT then begin

      Unpack_DLT(FStream, Str);

    end
    else if FFiles[Index].PackMethod = pmT64 then begin
      Temp := FFiles[Index].Extra; //Loading Offset
      Str.Write(Temp, 2);
      Str.CopyFrom(FStream, FFiles[Index].PackedSize);
    end
    else if FFiles[Index].PackMethod = pmRff then begin
      SetLength(Buf, 256);
      ReadLen := FStream.Read(Buf[0], 256);

      for i:=0 to ReadLen do Buf[i] := Buf[i] xor (i shr 1);

      Str.Write(Buf[0], ReadLen);

      Str.CopyFrom(FStream, FFiles[Index].PackedSize-ReadLen);
    end
    else if FFiles[Index].PackMethod = pmBzip2 then begin
       Dec2 := TDecompressBzip2Stream.Create(FStream);

       SetLength(Buf, 4096);
       repeat
         ReadLen := Dec2.Read(Buf[0], 4096);
         ProgStr.Write(Buf[0], ReadLen);

         if ReadLen < 4096 then break;
       until 1=0;

       Dec2.Free;
    end
    else if FFiles[Index].PackMethod = pmStore then begin

      ProgStr.CopyFrom(FStream, FFiles[Index].UnPackedSize);
    end
    else if FFiles[Index].PackMethod = pmLzma then begin
      //ExtractStreamLzma(FStream, Str);

      FStream.Read(Prop[0], 5);

      Lzma := TLZMADecoder.Create;
      Lzma.SetDecoderProperties(Prop);

      Lzma.Code(FStream, Str, FFiles[Index].UnpackedSize);

      Lzma.Free;
    end;

    ProgStr.Free;
    if FHasherClass <> nil then begin
      CalcCRC    := Hasher.Final;
      ArchiveCRC := IntToHex(FFiles[Index].CRC32, Length(CalcCRC));
      Hasher.Free;

      if ArchiveCRC = CalcCRC then Result := orVerified;
    end;
  except
    Result := orFail;
  end;
end;

constructor TUnpack.Create(Str: TStream);
begin
  FSize  := 1000;
  FCount := 0;
  SetLength(FFiles, FSize);
  FHasherClass := nil;
end;

destructor TUnpack.Destroy;
begin
  inherited Destroy;
end;

end.

