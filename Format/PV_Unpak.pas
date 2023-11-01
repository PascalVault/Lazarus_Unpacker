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
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  try
    FStream.Read(Head, SizeOf(Head));

    Total := Head.TableSize div 56;

    FStream.Position := Head.TableOffset;

    for i:=0 to Total-1 do begin

      FStream.Read(Entry, SizeOf(Entry));

      AFile.Name := Entry.FName;
      AFile.Offset := Entry.Offset;
      AFile.PackedSize := Entry.Size;
      AFile.UnpackedSize := Entry.Size;
      AFile.PackMethod := pmStore;
      AFile.ModDate := 0;
      AFile.CRC32 := 0;

      AddFile(AFile);
    end;
  except
  end;
end;

end.

