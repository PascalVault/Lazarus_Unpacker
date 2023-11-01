unit PV_Unyenc;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-20
//.YENC

interface

uses
  Classes, SysUtils, StrUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnyenc = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnyenc.Create(Str: TStream);
var Buf: String;
    i: Integer;
    BufLen: Integer;
    A,B: Integer;
    A2,B2,A3: Integer;
    PackSize: Integer;
    FName: String;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  try
    BufLen := FStream.Size - FStream.Position;
    SetLength(Buf, BufLen);
    FStream.Read(Buf[1], BufLen);

    A := Pos('=ybegin', Buf); //=ybegin line=128 size=FileSize name=FileName
    if A < 1 then Exit;

    A2 := Pos(#10, Buf, A);
    if A2 < 1 then Exit;

    A3 := Pos(' name=', Buf, A);
    if A3 < 1 then Exit;

    B := RPos('=yend', Buf);
    if B < 1 then Exit;

    PackSize := B-A2-1;

    FName := TrimRight(Copy(Buf, A3+6, A2-A3-6));

    AFile.Name := FName;
    AFile.Offset := A2;
    AFile.PackedSize := PackSize;
    AFile.UnpackedSize := PackSize;
    AFile.PackMethod := pmYENC;
    AFile.ModDate := 0;
    AFile.CRC32 := 0;

    AddFile(AFile);

  except
  end;
end;

end.

