unit PV_Unpack;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-19

interface

uses
  Classes, SysUtils, DateUtils, Math,
  ZStream, bzip2stream, ULZMADecoder, LzhHuff, LHA4, DLTUnpack, TxtEncodings, Dialogs;

type
  TPackMethod = (pmStore, pmDeflate, pmBzip2, pmLh1, pmLh4, pmLh5, pmLh6, pmLh7, pmLhX,
  pmLzma, pmT64, pmRff, pmUUE, pmXXE, pmB64, pmYenc, pmDLT, pmOther);

  TFile = record
    Name: String;
    PackedSize: Cardinal;
    UnpackedSize: Cardinal;
    ModDate: TDateTime;
    CRC32: Cardinal;
    Offset: Cardinal;
    PackMethod: TPackMethod;
    Extra: Cardinal;
  end;

  { TUnpack }

  TUnpack = class
  private
  public
    FFiles: array of TFile;
    FStream: TStream;
    FCount: Integer;
    FSize: Integer;
    procedure AddFile(AFile: TFile);
  public
    property Count: Integer read FCount;
    function GetName(Index: Integer): String;
    function GetSize(Index: Integer): Int64;
    function GetCRC(Index: Integer): Cardinal;
    function GetPackedSize(Index: Integer): Int64;
    function GetDateTime(Index: Integer): TDateTime;
    function CanUnpack(Index: Integer): Boolean;
    function Extract(Index: Integer; Str: TStream): Boolean;
    constructor Create(Str: TStream); virtual;
    destructor Destroy;
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


procedure TUnpack.AddFile(AFile: TFile);
begin
  FFiles[FCount] := AFile;
  Inc(FCount);

  if FCount = FSize then begin
    Inc(FSize, 1000);
    SetLength(FFiles, FSize);
  end;
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

function TUnpack.Extract(Index: Integer; Str: TStream): Boolean;
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
begin
  try
    Result := True;

    FStream.Position := FFiles[Index].Offset;

    if FFiles[Index].PackMethod = pmDeflate then begin
      Dec := TDecompressionStream.Create(FStream, True);
      ReadLen := Str.CopyFrom(Dec, FFiles[Index].UnpackedSize);
      Dec.Free;
    end
    else if FFiles[Index].PackMethod = pmLh1 then begin

      Lzh1 := TLZH.Create(FStream, Str);
      Lzh1.Decode(FFiles[Index].UnpackedSize);
      Lzh1.Free;

    end
    else if FFiles[Index].PackMethod in [pmLh4, pmLh5, pmLh6, pmLh7, pmLhX] then begin

      case FFiles[Index].PackMethod of
        pmLh4 : Lzh4 := TLha4.Create(FStream, Str);
        pmLh5 : Lzh4 := TLha5.Create(FStream, Str);
        pmLh6 : Lzh4 := TLha6.Create(FStream, Str);
        pmLh7 : Lzh4 := TLha7.Create(FStream, Str);
        pmLhX : Lzh4 := TLhaX.Create(FStream, Str);
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
         Str.Write(Buf[0], ReadLen);
         if ReadLen < 4096 then break;
       until 1=0;

       Dec2.Free;
    end
    else if FFiles[Index].PackMethod = pmStore then begin
      Str.CopyFrom(FStream, FFiles[Index].UnPackedSize);
    end
    else if FFiles[Index].PackMethod = pmLzma then begin
      //ExtractStreamLzma(FStream, Str);

      FStream.Read(Prop[0], 5);

      Lzma := TLZMADecoder.Create;
      Lzma.SetDecoderProperties(Prop);
      Lzma.Code(FStream, Str, FFiles[Index].UnpackedSize);
    end;
  except
    Result := False;
  end;
end;

constructor TUnpack.Create(Str: TStream);
begin
  FSize  := 1000;
  FCount := 0;
  SetLength(FFiles, FSize);
end;

destructor TUnpack.Destroy;
begin
  inherited Destroy;
end;

end.

