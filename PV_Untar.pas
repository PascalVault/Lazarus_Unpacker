unit PV_Untar;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-16

interface

uses
  Classes, SysUtils, PV_Unpack;

  { TUntar }

type
  TUntar = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUntar.Create(Str: TStream);
type THead = record
       FName: String;
       FileMode: Cardinal;
       OwnerID: Cardinal;
       GroupID: Cardinal;
       PackedSize: Cardinal;
       UnpackedSize: Cardinal;
       ModTime: Int64;
       Checksum: Cardinal;
       FileType: Byte;
       LinkName: String;
    end;

var Head: THead;
    Temp: AnsiString;
    Pos: Integer;
begin
  FStream := Str;
  SetLength(Temp, 200);

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;

    while True do begin

      FStream.Read(Temp[1], 100);
      Head.FName := StringReplace(Copy(Temp, 1, 100), chr(0), '', [rfReplaceAll]);

      if Head.FName = '' then break;

      FStream.Read(Temp[1], 8);
      Head.FileMode := StrToIntDef('&' + Copy(Temp, 1, 8), 0);

      FStream.Read(Temp[1], 8);
      Head.OwnerID := StrToIntDef('&' + Copy(Temp, 1, 8), 0);

      FStream.Read(Temp[1], 8);
      Head.GroupID := StrToIntDef('&' + Copy(Temp, 1, 8), 0);

      FStream.Read(Temp[1], 12);
      Head.UnpackedSize := StrToIntDef('&' + Copy(Temp, 1, 12), 0);

      FStream.Read(Temp[1], 12);
      Head.ModTime := StrToIntDef('&' + Copy(Temp, 1, 12), 0);

      FStream.Read(Temp[1], 8);
      Head.Checksum := StrToIntDef('&' + Copy(Temp, 1, 8), 0);

      FStream.Read(Temp[1], 1);
      Head.FileType := StrToIntDef(Temp[1], 0);

      FStream.Read(Temp[1], 100);
      Head.LinkName := Copy(Temp, 1, 100);

      Pos := 512 - (FStream.Position mod 512);

      FStream.Position := FStream.Position + Pos;

      Head.PackedSize := 512 - (Head.UnpackedSize mod 512) + Head.UnpackedSize;

      if FCount = FSize then begin
        Inc(FSize, 1000);
        SetLength(FFiles, FSize);
      end;

      FFiles[FCount].Name := Head.FName;
      FFiles[FCount].Offset := FStream.Position;
      FFiles[FCount].PackedSize := Head.UnPackedSize;
      FFiles[FCount].UnpackedSize := Head.UnpackedSize;
      FFiles[FCount].PackMethod := pmStore;

      FStream.Position := FStream.Position + Head.PackedSize;
      Inc(FCount);

      if FStream.Position+1024 >= FStream.Size then break;

    end;
  except
  end;
end;

end.

