unit PV_Unt64;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-14
//Commodore 64 .T64

//odczyt plików jest dziwny, są one jakby w róznych miejsach T64 zapisane

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnt64 = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnt64.Create(Str: TStream);
type THead = packed record
      Magic: array[0..31] of Char;
      Version: Word;
      Entries: Word;
      UsedEntries: Word;
      Free: Word;
      Description: array[0..23] of Char;
      //64 bytes total
    end;
    TEntry = packed record
      EntryType: Byte;
      FileType: Byte;
      StartOffset: Word;
      EndOffset: Word;
      Free2: Word;
      FileOffset: Cardinal;
      Free: Cardinal;
      Filename: array[0..15] of Char;
      //28 bytes
    end;

var Head: THead;
    Entry: TEntry;
    i: Integer;
    FName: String;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  try
    FStream.Read(Head, SizeOf(Head));

    for i:=0 to Head.Entries-1 do begin

      FStream.Read(Entry, SizeOf(Entry));

      AFile.Name := Entry.Filename;
      AFile.Offset := Entry.FileOffset;
      AFile.PackedSize := Entry.EndOffset - Entry.StartOffset;
      AFile.UnpackedSize := Entry.EndOffset - Entry.StartOffset+2;
      AFile.Extra := Entry.StartOffset;
      AFile.PackMethod := pmT64;
      AFile.ModDate := 0;
      AFile.CRC32 := 0;

      AddFile(AFile);
    end;
  except
  end;
end;

end.

