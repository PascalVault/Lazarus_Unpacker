unit PV_Unpack;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-19

interface

uses
  Classes, SysUtils, ZStream, bzip2stream, ULZMADecoder, LzhHuff, Dialogs;

type
  TPackMethod = (pmStore, pmDeflate, pmBzip2, pmLh1, pmLzma, pmT64, pmRff, pmOther);

  TFile = record
    Name: String;
    PackedSize: Cardinal;
    UnpackedSize: Cardinal;
    Offset: Cardinal;
    PackMethod: TPackMethod;
    Extra: Cardinal;
  end;

  { TUnpack }

  TUnpack = class
  public
    FFiles: array of TFile;
    FStream: TStream;
    FCount: Integer;
    FSize: Integer;
  public
    property Count: Integer read FCount;
    function GetName(Index: Integer): String;
    function GetSize(Index: Integer): Int64;
    function GetPackedSize(Index: Integer): Int64;
    function CanUnpack(Index: Integer): Boolean;
    function Extract(Index: Integer; Str: TStream): Boolean;
    constructor Create(Str: TStream); virtual; abstract;
    destructor Destroy;
  end;

  function ReadStrNull(Str: TStream): String;

implementation

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



function TUnpack.GetName(Index: Integer): String;
begin
  Result := FFiles[Index].Name;
end;

function TUnpack.GetSize(Index: Integer): Int64;
begin
  Result := FFiles[Index].UnpackedSize;
end;

function TUnpack.GetPackedSize(Index: Integer): Int64;
begin
  Result := FFiles[Index].PackedSize;
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
    Lzh: TLZH;
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

      Lzh := TLZH.Create(FStream, Str);
      Lzh.Decode(FFiles[Index].UnpackedSize);
      Lzh.Free;

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
      Str.CopyFrom(FStream, FFiles[Index].UnpackedSize);
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

destructor TUnpack.Destroy;
begin
  inherited Destroy;
end;

end.

