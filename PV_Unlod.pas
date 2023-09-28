unit PV_Unlod;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-18
//HoMM3 .LOD

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnlod = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnlod.Create(Str: TStream);
type THead = packed record
      Magic: array[0..3] of Char;
      Unknown: Cardinal;
      TableCount: Cardinal;
    end;
    TEntry = packed record
      FName: array[0..15] of Char;
      Offset: Cardinal;
      UnpackedSize: Cardinal;
      Typee: Cardinal;
      PackedSize: Cardinal;
    end;

var Head: THead;
    Entry: TEntry;
    i: Integer;
begin
  FStream := Str;

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;

    FStream.Read(Head, SizeOf(Head));

    FStream.Position := FStream.Position + 80;

    for i:=0 to Head.TableCount-1 do begin

      FStream.Read(Entry, SizeOf(Entry));

      if FCount = FSize then begin
        Inc(FSize, 1000);
        SetLength(FFiles, FSize);
      end;

      FFiles[FCount].Name := Entry.FName;
      FFiles[FCount].Offset := Entry.Offset+2;         //2 bytes= zlib header
      FFiles[FCount].PackedSize := Entry.PackedSize-6; //4 bytes=zlib footer+2 bytes=zlib header
      FFiles[FCount].UnpackedSize := Entry.UnpackedSize;
      FFiles[FCount].PackMethod := pmDeflate;

      Inc(FCount);
    end;
  except
  end;
end;

end.

