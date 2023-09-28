unit PV_Unpak;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-18
//Quake .PAK

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnpak = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnpak.Create(Str: TStream);
type THead = packed record
      Magic: array[0..3] of Char;
      TableOffset: Cardinal;
      TableSize: Cardinal;
    end;
    TEntry = packed record
      FName: array[0..55] of Char;
      Offset: Cardinal;
      Size: Cardinal;
    end;

var Head: THead;
    Entry: TEntry;
    i: Integer;
    Total: Integer;
begin
  FStream := Str;

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;

    FStream.Read(Head, SizeOf(Head));

    Total := Head.TableSize div 56;

    FStream.Position := Head.TableOffset;

    for i:=0 to Total-1 do begin

      FStream.Read(Entry, SizeOf(Entry));

      if FCount = FSize then begin
        Inc(FSize, 1000);
        SetLength(FFiles, FSize);
      end;

      FFiles[FCount].Name := Entry.FName;
      FFiles[FCount].Offset := Entry.Offset;
      FFiles[FCount].PackedSize := Entry.Size;
      FFiles[FCount].UnpackedSize := Entry.Size;
      FFiles[FCount].PackMethod := pmStore;

      Inc(FCount);
    end;
  except
  end;
end;

end.

