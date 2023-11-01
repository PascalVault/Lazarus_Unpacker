unit PV_Unhog;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-15
//Descent .HOG

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnhog = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnhog.Create(Str: TStream);
type THead = packed record
      Magic: array[0..2] of Char; //"DHF"
    end;
    TEntry = packed record
      FName: array[0..12] of Char;
      Size: Cardinal;
    end;

var Head: THead;
    Entry: TEntry;
    i: Integer;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  try
    FStream.Read(Head, SizeOf(Head));

    while FStream.Position < FStream.Size do begin

      FStream.Read(Entry, SizeOf(Entry));

      AFile.Name := Entry.FName;
      AFile.Offset := FStream.Position;
      AFile.PackedSize := Entry.Size;
      AFile.UnpackedSize := Entry.Size;
      AFile.PackMethod := pmStore;
      AFile.ModDate := 0;
      AFile.CRC32 := 0;

      AddFile(AFile);

      FStream.Position := FStream.Position + Entry.Size;

    end;

  except
  end;
end;

end.

