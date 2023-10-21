unit PV_Unpack;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-19

interface

uses
  Classes, SysUtils, ZStream, bzip2stream, ULZMADecoder, LzhHuff, Math, Dialogs;

type
  TPackMethod = (pmStore, pmDeflate, pmBzip2, pmLh1, pmLzma, pmT64, pmRff, pmUUE, pmXXE, pmB64, pmYenc, pmOther);

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



procedure DecodeB64(InStr, OutStr: TStream; ALength: Integer);
const CharsTab: String = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var Chars: array[0..255] of Byte;
    List: TStringList;
    Buf: String;
    DecLine, Line: String;
    LineB: array of Byte absolute Line;
    Val: Cardinal;
    ValC: array[0..3] of Char absolute Val;
    Len: Integer;
    k,j: Integer;
begin
  //convert char tab- for speed-up
  for k:=0 to 255 do Chars[k] := 0;
  for k:=1 to Length(CharsTab) do
    Chars[ord(CharsTab[k])] := k-1;

  SetLength(Buf, ALength);
  InStr.Read(Buf[1], ALength);

  List := TStringList.Create;
  List.Text := Buf;
  Buf := '';


  DecLine := '';
  for k:=0 to List.Count-1 do begin
    if List[k] = '' then continue;

    Line := List[k];
    Len := Length(Line);

    j := 0;
    while j < Len do begin
      Val := (Chars[ LineB[j+0] ] shl 18) +
             (Chars[ LineB[j+1] ] shl 12) +
             (Chars[ LineB[j+2] ] shl  6) +
              Chars[ LineB[j+3] ];

      DecLine := DecLine + ValC[2] + ValC[1] + ValC[0];

      Inc(j,4);
    end;
  end;
  List.Free;

  OutStr.Write(DecLine[1], Length(DecLine));
end;

procedure DecodeYENC(InStr, OutStr: TStream; ALength: Integer);
var Ch: Byte;
    Buf: String;
    DecLine: String;
    i: Integer;
begin
  SetLength(Buf, ALength);
  InStr.Read(Buf[1], ALength);

  DecLine := '';

  i := 1;
  while i <= ALength do begin
    Ch := ord(Buf[i]);

    if Ch = ord('=') then begin
      DecLine := DecLine + chr(ord(Buf[i+1])-106);
      Inc(i);
    end
    else if (Ch <> 13) and (Ch <> 10) then DecLine := DecLine + chr(Ch - 42);

    Inc(i);
  end;

  OutStr.Write(DecLine[1], Length(DecLine));
end;

procedure DecodeXXE(InStr, OutStr: TStream; ALength: Integer);
const CharsTab: String = '+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
var Chars: array[0..255] of Byte;
    List: TStringList;
    Buf: String;
    DecLine, Line: String;
    LineB: array of Byte absolute Line;
    Val: Cardinal;
    ValC: array[0..3] of Char absolute Val;
    Len: Integer;
    k,j: Integer;
begin
  //convert char tab- for speed-up
  for k:=0 to 255 do Chars[k] := 0;
  for k:=1 to Length(CharsTab) do
    Chars[ord(CharsTab[k])] := k-1;

  SetLength(Buf, ALength);
  InStr.Read(Buf[1], ALength);

  List := TStringList.Create;
  List.Text := Buf;
  Buf := '';


  DecLine := '';
  for k:=0 to List.Count-1 do begin
    if List[k] = '' then continue;
    if List[k] = '+' then break;

    Line := List[k];
    Len := Chars[LineB[0]];

    j := 1;
    while j<Ceil(4*Len/3) do begin
      Val := (Chars[ LineB[j+0] ] shl 18) +
             (Chars[ LineB[j+1] ] shl 12) +
             (Chars[ LineB[j+2] ] shl  6) +
              Chars[ LineB[j+3] ];

      DecLine := DecLine + ValC[2] + ValC[1] + ValC[0];

      Inc(j,4);
    end;
  end;
  List.Free;

  OutStr.Write(DecLine[1], Length(DecLine));
end;

procedure DecodeUUE(InStr, OutStr: TStream; ALength: Integer);
const CharsTab: String = ' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_';
var Chars: array[0..255] of Byte;
    List: TStringList;
    Buf: String;
    DecLine, Line: String;
    LineB: array of Byte absolute Line;
    Val: Cardinal;
    ValC: array[0..3] of Char absolute Val;
    Len: Integer;
    k,j: Integer;
begin
  //convert char tab- for speed-up
  for k:=0 to 255 do Chars[k] := 0;
  for k:=1 to Length(CharsTab) do
    Chars[ord(CharsTab[k])] := k-1;

  SetLength(Buf, ALength);
  InStr.Read(Buf[1], ALength);

  List := TStringList.Create;
  List.Text := Buf;
  Buf := '';

  DecLine := '';
  for k:=0 to List.Count-1 do begin
    if List[k] = '' then continue;
    if List[k] = '`' then break;

    Line := List[k];
    Len := Chars[LineB[0]];

    j := 1;
    while j<Ceil(4*Len/3) do begin
      Val := (Chars[ LineB[j+0] ] shl 18) +
             (Chars[ LineB[j+1] ] shl 12) +
             (Chars[ LineB[j+2] ] shl  6) +
              Chars[ LineB[j+3] ];

      DecLine := DecLine + ValC[2] + ValC[1] + ValC[0];

      Inc(j,4);
    end;
  end;
  List.Free;

  OutStr.Write(DecLine[1], Length(DecLine));
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

