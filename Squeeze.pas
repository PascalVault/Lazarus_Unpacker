unit Squeeze;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Based on usq by Dick Greenlaw (~1983)
//Licence: MIT
//Last update: 2023-11-12

interface

uses
  Classes, SysUtils;

const
    numvals    = 257;      //max size of tree + 1
    speof      = 256;      //marker for end of file
    dle: char  = #$90;
type
    tree       = array[0..255, 0..1] of Int16;

type

  { TSqueeze }

  TSqueeze = class
  private
    dnode: tree;
    curin, bpos, repct, numnodes: int16;
    c, lastchar: ansichar;
    DataEnd: Boolean;

    FInStream: TStream;
    FOutStream: TStream;

    function ReadChar: Int16;
    function ReadWord: Int16;
    procedure WriteChar(ch: ansichar);
    function Unhuff: ansichar;
    function DecodeChar: ansichar;
  public
    constructor Create(InStream, OutStream: TStream);
    procedure Decode(PackedSize: Int64);
  end;

implementation

constructor TSqueeze.Create(InStream, OutStream: TStream);
begin
  FInStream := InStream;
  FOutStream := OutStream;
end;

function TSqueeze.ReadChar: Int16;
var tmp: byte;
begin
  FInStream.Read(tmp, 1);
  result := tmp;
end;

function TSqueeze.ReadWord: Int16;
 var tmp: Word;
begin
  FInStream.Read(tmp, 2);
  result := tmp;
end;

procedure TSqueeze.WriteChar(ch: ansichar);
var tmp: Byte;
begin
  tmp := ord(ch);
  FOutStream.Write(tmp, 1);
end;

function TSqueeze.Unhuff: ansichar;
var i: Int16;
begin
  i := 0;
  repeat
    inc(bpos);
    if bpos > 7 then begin
      curin := ReadChar;
      bpos := 0;
      i := ord(dnode[i,ord(1 and curin)]);
    end
    else begin
      curin := curin shr 1;
      i := ord(dnode[i,ord(1 and curin)]);
    end;
  until (i < 0);

  i := -(i + 1);
  if i = speof then begin
    DataEnd := true;
    result := chr(26)
  end
  else result := chr(i);
end;

function TSqueeze.DecodeChar: ansichar;
var ch: ansichar;
begin
  if repct > 0 then begin
    dec(repct);
    exit(lastchar);
  end;
  ch := Unhuff;
  if ch <> dle then begin
    result := ch;
    lastchar := ch;
    exit;
  end;
  repct := ord(Unhuff);
  if repct = 0 then exit(dle);

  dec(repct, 2);
  result := lastchar;
end;

procedure TSqueeze.Decode(PackedSize: Int64);
var i: Integer;
begin
  repct := 0;
  bpos := 99;
  DataEnd := false;

  numnodes := ReadWord;

  if (numnodes < 0) or (numnodes >= numvals) then begin
    raise Exception.Create('Invalid number of nodes');
    Exit;
  end;

  dnode[0,0] := -(speof+1);
  dnode[0,1] := -(speof+1);
  numnodes := numnodes-1;

  for i:=0 to numnodes  do begin
    dnode[i,0] := ReadWord;
    dnode[i,1] := ReadWord;
  end;

  while FInStream.Position < FInStream.Size do begin
    c := DecodeChar;

    if DataEnd then break;

    WriteChar(c);
  end;
end;

end.

