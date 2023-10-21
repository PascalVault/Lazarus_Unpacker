unit PV_Unpacker;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-16

interface

uses
  Classes, SysUtils, Bufstream, Dialogs, PV_Unpack;

type
  TUnpackClass = class of TUnpack;

  { TUnpacker }

  TUnpacker = class
  private
    FHandle: TBufferedFileStream;
    FObj: TUnpack;
    function Detect(Buff: array of Byte): TUnpackClass; overload;
    function Detect(Str: TStream): TUnpackClass; overload;
    procedure Init;
    procedure Open(Str: TStream); overload;
    procedure Open(Filename: String); overload;
  public
    function GetFormat: String;
    function Count: Integer;
    function GetName(Index: Integer): String;
    function GetSize(Index: Integer): Int64;
    function GetPackedSize(Index: Integer): Int64;
    function CanUnpack(Index: Integer): Boolean;
    function Extract(Index: Integer; Str: TStream): Boolean; overload;
    function Extract(Index: Integer; Filename: String): Boolean; overload;
    constructor Create(Str: TStream); overload;
    constructor Create(Filename: String); overload;
    destructor Destroy;
  end;



implementation

uses PV_Unzip, PV_Untar, PV_Unrar, PV_Unrar5, PV_Unlzh, PV_Unpak, PV_Ungzip, PV_Unbh, PV_Unwad, PV_Unwad2,
     PV_Unbzip2, PV_Unbza, PV_Unlod, PV_Unarj, PV_Unftg, PV_Undrs, PV_Unlzma, PV_Unbin, PV_Ungrp, PV_Ungw3, PV_Unhog,
     PV_Unbig, PV_Uncpio, PV_Undpk, PV_Unha, PV_Unlbr, PV_Unpck, PV_Unalz, PV_Unegg, PV_Unlzx, PV_Unrpm, PV_Unt64,
     PV_UnRsc, PV_UnRff, PV_UnPcl, PV_UnLib, PV_UnLib2, PV_UnGx, PV_UnEpf, PV_UnDlt, PV_UnDat, PV_Unxxe, PV_Unuue,
     PV_Unarc, PV_Unzoo, PV_Unyenc, PV_Unb64;

function TUnpacker.Detect(Buff: array of Byte): TUnpackClass;

function isLZMA(B1, B2, B3, B4: Byte): Boolean;
var DictSize: Cardinal;
    i: Integer;
begin
  //2^16 ... 2^25
  //2^n = 1 << n
  Result := False;

  DictSize := B1 + (B2 shl 8) + (B3 shl 16) + (B4 shl 24);

  for i:=16 to 25 do begin
    if DictSize = (1 shl i) then Exit(True);
  end;
end;

begin
  if (Buff[  3] = $04) and (Buff[  2] = $03) and (Buff[  1] = $4b) and (Buff[  0] = $50) then Exit(TUnzip);

  if (Buff[  0] = $52) and (Buff[  1] = $61) and (Buff[  2] = $72) and (Buff[  3] = $21) and
     (Buff[  4] = $1A) and (Buff[  5] = $07) then
  begin
    if      (Buff[  6] = $01) and (Buff[7] = $00) then Exit(TUnrar5)
    else if (Buff[  6] = $00)                     then Exit(TUnrar);
  end;

  if (Buff[  2] = $2D) and (Buff[  3] = $6C) and (Buff[ 4] = $68) and (Buff[ 6] = $2D) then Exit(TUnlzh);

  if (Buff[  0] = ord('P')) and (Buff[  1] = ord('A')) and (Buff[ 2] = ord('C')) and (Buff[ 3] = ord('K')) then Exit(TUnpak);

  if (Buff[  0] = ord('I')) and (Buff[  1] = ord('W')) and (Buff[ 2] = ord('A')) and (Buff[ 3] = ord('D')) then Exit(TUnwad);

  if (Buff[  0] = ord('P')) and (Buff[  1] = ord('W')) and (Buff[ 2] = ord('A')) and (Buff[ 3] = ord('D')) then Exit(TUnwad);

  if (Buff[  0] = ord('W')) and (Buff[  1] = ord('A')) and (Buff[ 2] = ord('D')) and (Buff[ 3] = ord('2')) then Exit(TUnwad2);

  if (Buff[  0] = $1f) and (Buff[  1] = $8b) and (Buff[ 2] = 8) then Exit(TUngzip);

  if (Buff[  0] = ord('B')) and (Buff[  1] = ord('H')) then Exit(TUnbh);

  if (Buff[  0] = ord('B')) and (Buff[  1] = ord('Z')) and (Buff[  2] = ord('h')) then Exit(TUnbzip2);

  //if (Buff[  0] = $FD) and (Buff[  1] = $37) and (Buff[ 2] = $7A) and (Buff[ 3] = $58) then Exit(TUnxz);

  //if (Buff[  0] = $37) and (Buff[  1] = $7A) and (Buff[ 2] = $BC) and (Buff[ 3] = $AF) then Exit(TUn7zip);


  if (Buff[  4] = ord('G')) and (Buff[  5] = ord('Z')) and (Buff[ 6] = ord('I')) and (Buff[ 7] = ord('P')) then Exit(TUnbza);
  if (Buff[  4] = ord('B')) and (Buff[  5] = ord('Z')) and (Buff[ 6] = ord('2')) and (Buff[ 7] = 0       ) then Exit(TUnbza);

  if (Buff[  0] = ord('L')) and (Buff[  1] = ord('O')) and (Buff[ 2] = ord('D')) and (Buff[ 3] = 0       ) then Exit(TUnlod);

  if (Buff[$64] = $30) and (Buff[$6c] = $30) and (Buff[$74] = $30) and (Buff[$7c] = $30) then Exit(TUntar);

  if (Buff[  0] = ord('B')) and (Buff[  1] = ord('O')) and (Buff[ 2] = ord('T')) and (Buff[ 3] = ord('G')) then Exit(TUnftg);

  if (Buff[19] = ord('E')) and (Buff[  20] = ord('n')) and (Buff[21] = ord('s')) and (Buff[22] = ord('e')) then Exit(TUndrs);

  if (Buff[  0] = ord('B')) and (Buff[  1] = ord('I')) and (Buff[ 2] = ord('G')) and (Buff[ 3] = ord('F')) then Exit(TUnbig);
  if (Buff[  0] = ord('B')) and (Buff[  1] = ord('I')) and (Buff[ 2] = ord('G')) and (Buff[ 3] = ord('H')) then Exit(TUnbig);

  if (Buff[  0] = ord('0')) and (Buff[  1] = ord('7')) and (Buff[ 2] = ord('0')) and (Buff[ 3] = ord('7')) then Exit(TUncpio);

  if (Buff[  0] = ord('P')) and (Buff[  1] = ord('C')) and (Buff[ 2] = ord('K')) and (Buff[ 3] = ord('F')) then Exit(TUnpck);

  if (Buff[  0] = ord('P')) and (Buff[  1] = ord('A')) then Exit(TUndpk);

  if (Buff[  0] = ord('H')) and (Buff[  1] = ord('A')) then Exit(TUnha);

  if (Buff[  0] =   $1A   ) and (Buff[  1] in [2,3,4,5,6,7,8,9]) then Exit(TUnARC);

  if (Buff[  0] = ord('C')) and (Buff[  1] = ord('6')) and (Buff[ 2] = ord('4')) and (Buff[31] = 0) then Exit(TUnT64);

  if (Buff[  0] = ord('A')) and (Buff[  1] = ord('L')) and (Buff[ 2] = ord('Z')) then Exit(TUnAlz);

  if (Buff[  0] = ord('L')) and (Buff[  1] = ord('Z')) and (Buff[ 2] = ord('X')) then Exit(TUnLZX);

  if (Buff[  0] = $ED) and (Buff[  1] = $AB) and (Buff[ 2] = $EE) and (Buff[ 2] = $DB) then Exit(TUnRPM);

  if (Buff[  0] = ord('D')) and (Buff[  1] = ord('H')) and (Buff[ 2] = ord('F')) then Exit(TUnHog);

  if (Buff[  0] = ord('Z')) and (Buff[  1] = ord('O')) and (Buff[ 2] = ord('O')) then Exit(TUnZOO);

  if (Buff[  0] = ord('U')) and (Buff[  1] = ord('U')) and (Buff[ 2] = ord('E')) then Exit(TUnUUE);

  if (Buff[  0] = ord('X')) and (Buff[  1] = ord('X')) and (Buff[ 2] = ord('E')) then Exit(TUnXXE);

  if (Buff[  0] = ord('D')) and (Buff[  1] = ord('A')) and (Buff[ 2] = ord('T')) then Exit(TUnDat);

  if (Buff[  0] = ord('L')) and (Buff[  1] = ord('I')) and (Buff[ 2] = ord('B')) then Exit(TUnLib);

  if (Buff[  0] = ord('D')) and (Buff[  1] = ord('A')) and (Buff[  2] = ord('V')) and (Buff[  3] = ord('E')) then Exit(TUnDlt);

  if (Buff[  0] = ord('E')) and (Buff[  1] = ord('P')) and (Buff[  2] = ord('F')) and (Buff[  3] = ord('S')) then Exit(TUnEpf);

  if (Buff[  0] = ord('E')) and (Buff[  1] = ord('A')) and (Buff[  2] = ord('L')) and (Buff[  3] = ord('I')) then Exit(TUnLib2);

  if (Buff[  0] = ord('R')) and (Buff[  1] = ord('F')) and (Buff[ 2] = ord('F')) and (Buff[ 3] = $1A) then Exit(TUnRff);

  if (Buff[  0] = ord('=')) and (Buff[  1] = ord('y')) and (Buff[  2] = ord('b')) and (Buff[  3] = ord('e')) then Exit(TUnYenc);


  if (Buff[  0] = ord('P')) and (Buff[  1] = ord('R')) and (Buff[  2] = ord('O')) and (Buff[  3] = ord('L')) then Exit(TUnRsc);

  if (Buff[  0] = ord('p')) and (Buff[  1] = ord('c')) and (Buff[  2] = ord('x')) and (Buff[  3] = ord('L')) then Exit(TUnPcl);

  if (Buff[  0] = ord('C')) and (Buff[  1] = ord('S')) and (Buff[  2] = ord('i')) and (Buff[  3] = ord('d')) then Exit(TUnbin);

  if (Buff[  0] = ord('K')) and (Buff[  1] = ord('e')) and (Buff[  2] = ord('n')) and (Buff[  3] = ord('S')) then Exit(TUngrp);

  if (Buff[  0] = ord('E')) and (Buff[  1] = ord('G')) and (Buff[ 2] = ord('G')) and (Buff[ 3] = ord('A')) then Exit(TUnEgg);

  if (Buff[  0] = ord('H')) and (Buff[  1] = ord('o')) and (Buff[ 9] = ord('F')) and (Buff[14] = ord('F')) then Exit(TUnGw3);

  if (Buff[  0] = 0       ) and (Buff[  1] = ord(' ')) and (Buff[ 2] = ord(' ')) and (Buff[ 3] = ord(' ')) then Exit(TUnlbr);

  if (Buff[  0] < 5*5*9   ) and (Buff[ 13] = 0) and (isLZMA(Buff[1], Buff[2], Buff[3], Buff[4])) then Exit(TUnlzma);


  if (Buff[  0] = ord('B')) and (Buff[  1] = ord('a')) and (Buff[ 2] = ord('s')) and (Buff[ 3] = ord('e')) then Exit(TUnB64);

  if (Buff[  0] = $60) and (Buff[  1] = $ea) then Exit(TUnarj);

  if (Buff[  0] = $01) and (Buff[  1] = $CA) then Exit(TUnGx);

  Result := nil;
end;

function  TUnpacker.Detect(Str: TStream): TUnpackClass;
var Buff: array[0..199] of Byte;
    Len: Integer;
begin
  Len := Str.Read(Buff, 200);
  Str.Position := Str.Position - Len;

  Result := Detect(Buff);
end;

function TUnpacker.GetFormat: String;
begin
  if FObj = nil then Exit('');

  Result := Copy(FObj.ClassName, 4);
end;

function TUnpacker.Count: Integer;
begin
  if FObj = nil then Exit(-1);

  Result := FObj.Count;
end;

function TUnpacker.GetName(Index: Integer): String;
begin
  if FObj = nil then Exit('');

  Result := FObj.GetName(Index);
end;

function TUnpacker.GetSize(Index: Integer): Int64;
begin
  if FObj = nil then Exit(-1);

  Result := FObj.GetSize(Index);
end;

function TUnpacker.GetPackedSize(Index: Integer): Int64;
begin
  if FObj = nil then Exit(-1);

  Result := FObj.GetPackedSize(Index);
end;

function TUnpacker.CanUnpack(Index: Integer): Boolean;
begin
  if FObj = nil then Exit(False);

  Result := FObj.CanUnpack(Index);
end;

function TUnpacker.Extract(Index: Integer; Str: TStream): Boolean;
begin
  if FObj = nil then Exit(False);

  Result := FObj.Extract(Index, Str);
end;

function TUnpacker.Extract(Index: Integer; Filename: String): Boolean;
var F: TFileStream;
begin
  Result := False;
  if FObj = nil then Exit;

  try
    F := TFileStream.Create(Filename, fmCreate or fmShareDenyWrite);
    Result := Extract(Index, F);
  finally
    F.Free;
  end;
end;

procedure TUnpacker.Init;
begin
  inherited Create;

  FHandle := nil;
  FObj := nil;
end;

procedure TUnpacker.Open(Str: TStream);
var FClass: TUnpackClass;
begin
  FClass := Detect(Str);

  if FClass = nil then Exit;

  FObj := FClass.Create(Str);
end;

procedure TUnpacker.Open(Filename: String);
begin
  FHandle := TBufferedFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);

  Open(FHandle);
end;

constructor TUnpacker.Create(Str: TStream);
begin
  Init;
  Open(Str);
end;

constructor TUnpacker.Create(Filename: String);
begin
  Init;
  Open(Filename);
end;

destructor TUnpacker.Destroy;
begin
  if FHandle <> nil then FHandle.Free;
  inherited Destroy;
end;


end.

