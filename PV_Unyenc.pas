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
begin
  FStream := Str;

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;

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


    if FCount = FSize then begin
      Inc(FSize, 1000);
      SetLength(FFiles, FSize);
    end;

    FFiles[FCount].Name := FName;
    FFiles[FCount].Offset := A2;
    FFiles[FCount].PackedSize := PackSize;
    FFiles[FCount].UnpackedSize := PackSize;
    FFiles[FCount].PackMethod := pmYENC;

    Inc(FCount);

  except
  end;
end;

end.

