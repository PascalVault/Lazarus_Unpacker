unit PV_Undat;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-15
//Shaws Nightmare .DAT

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUndat = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUndat.Create(Str: TStream);
type THead = packed record
      Magic: array[0..3] of Char; //"DAT" + $1A
      Count: Word;
      Offset: Cardinal;
      IndexSize: Cardinal;
    end;
    TEntry = packed record
      FName: array[0..12] of Char;
      Padding: array[0..2] of Char;
      Size: Cardinal;
      Offset: Cardinal;
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

    FStream.Position := Head.Offset;

    for i:=0 to Head.Count-1 do begin

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

