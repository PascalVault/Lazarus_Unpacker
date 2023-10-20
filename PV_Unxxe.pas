unit PV_Unxxe;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-20
//XXEncode .XXE

interface

uses
  Classes, SysUtils, StrUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnxxe = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnxxe.Create(Str: TStream);
type THead = packed record
      Magic: array[0..2] of Char; //xxe
    end;

var Head: THead;
    Buf: String;
    i: Integer;
    BufLen: Integer;
    A,B: Integer;
    A2,B2: Integer;
    PackSize: Integer;
    FName: String;
begin
  FStream := Str;

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;

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

    if FCount = FSize then begin
      Inc(FSize, 1000);
      SetLength(FFiles, FSize);
    end;

    FFiles[FCount].Name := FName;
    FFiles[FCount].Offset := A2+SizeOf(Head);
    FFiles[FCount].PackedSize := PackSize;
    FFiles[FCount].UnpackedSize := Round(PackSize * 3 / 4);
    FFiles[FCount].PackMethod := pmXXE;

    Inc(FCount);

  except
  end;
end;

end.

