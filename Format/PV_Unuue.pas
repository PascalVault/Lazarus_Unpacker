unit PV_Unuue;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-20
//UUEncode .UUE

interface

uses
  Classes, SysUtils, StrUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnuue = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnuue.Create(Str: TStream);
type THead = packed record
      Magic: array[0..2] of Char; //UUE
    end;

var Head: THead;
    Buf: String;
    i: Integer;
    BufLen: Integer;
    A,B: Integer;
    A2,B2: Integer;
    PackSize: Integer;
    FName: String;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  try
    FStream.Read(Head, SizeOf(Head));

    BufLen := FStream.Size - FStream.Position;
    SetLength(Buf, BufLen);
    FStream.Read(Buf[1], BufLen);

    A := Pos('begin ', Buf); //begin 644 FileName
    if A < 1 then Exit;

    A2 := Pos(#10, Buf, A);
    if A2 < 1 then Exit;

    B := RPos('end', Buf);
    if B < 1 then Exit;

    PackSize := B-A2;

    FName := TrimRight(Copy(Buf, A+10, A2-A-10));

    AFile.Name := FName;
    AFile.Offset := A2+SizeOf(Head);
    AFile.PackedSize := PackSize;
    AFile.UnpackedSize := Round(PackSize * 3 / 4);
    AFile.PackMethod := pmUUE;
    AFile.ModDate := 0;
    AFile.CRC32 := 0;

    AddFile(AFile);

  except
  end;
end;

end.

