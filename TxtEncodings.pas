unit TxtEncodings;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-25

interface

uses
  Classes, SysUtils, DateUtils, Math, Dialogs;

  procedure DecodeB64(InStr, OutStr: TStream; ALength: Integer);
  procedure DecodeYENC(InStr, OutStr: TStream; ALength: Integer);
  procedure DecodeXXE(InStr, OutStr: TStream; ALength: Integer);
  procedure DecodeUUE(InStr, OutStr: TStream; ALength: Integer);

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

end.
