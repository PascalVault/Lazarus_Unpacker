unit PV_Unb64;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-21
//Base64 .b64

interface

uses
  Classes, SysUtils, StrUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnb64 = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnb64.Create(Str: TStream);
type THead = packed record
      Magic: array[0..2] of Char; //Bas
    end;

var Head: THead;
    Buf: String;
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
    FStream.Read(Head, SizeOf(Head));

    BufLen := FStream.Size - FStream.Position;
    SetLength(Buf, BufLen);
    FStream.Read(Buf[1], BufLen);

    A := Pos('attachment;', Buf); //attachment; filename="FileName"
    if A < 1 then Exit;

    A2 := Pos(#10, Buf, A);
    if A2 < 1 then Exit;

    A3 := Pos('filename="', Buf, A);
    if A3 < 1 then Exit;

    B := RPos('end', Buf);
    if B < 1 then Exit;

    PackSize := B-A2-1;

    FName := TrimRight(Copy(Buf, A3+10, A2-A3-10-1));

    AFile.Name := FName;
    AFile.Offset := A2+SizeOf(Head);
    AFile.PackedSize := PackSize;
    AFile.UnpackedSize := Round(PackSize * 3 / 4);
    AFile.PackMethod := pmB64;
    AFile.ModDate := 0;
    AFile.CRC32 := 0;

    AddFile(AFile);

  except
  end;
end;

end.

