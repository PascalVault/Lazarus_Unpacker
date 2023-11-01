unit PV_Unlib2;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-15
//Car and Driver .LIB

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnlib2 = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnlib2.Create(Str: TStream);
type THead = packed record
      Magic: array[0..4] of Char; //"EALIB"
      Count: Word; 
    end;
    TEntry = packed record
      FName: array[0..12] of Char;
      Unk: Byte;
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

    for i:=0 to Head.Count-2 do begin

      FStream.Read(Entry, SizeOf(Entry));

      AFile.Name := Entry.FName;
      AFile.Offset := Entry.Offset;
      AFile.PackedSize := 0;
      AFile.UnpackedSize := 0;
      AFile.PackMethod := pmStore;
      AFile.ModDate := 0;
      AFile.CRC32 := 0;

      AddFile(AFile);
    end;

    for i:=0 to Head.Count-2 do begin
      FFiles[i].PackedSize   := FFiles[i+1].Offset - FFiles[i].Offset;
      FFiles[i].UnpackedSize := FFiles[i].PackedSize;
    end;

  except
  end;
end;

end.

