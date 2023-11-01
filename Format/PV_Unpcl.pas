unit PV_Unpcl;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-15
//The Oregon Trail .PCL

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnpcl = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnpcl.Create(Str: TStream);
type THead = packed record
      Magic: array[0..9] of Char; //"pcxLib"
      Copyright: array[0..49] of Char;
      Version: Word;
      Caption: array[0..39] of Char;
      Padding: array[0..19] of Char; 
    end;
    TEntry = packed record
      Synch: Byte;
      FName: array[0..12] of Char;
      Size: Cardinal;
      FileDate: Word;
      FileTime: Word;
      Method: Word;
      Note: array[0..39] of Char;
      Padding: array[0..19] of Char;  
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
      AFile.ModDate := Dos2DateTime(Entry.FileTime, Entry.FileDate);
      AFile.CRC32 := 0;

      AddFile(AFile);

      FStream.Position := FStream.Position + Entry.Size;
    end;

  except
  end;
end;

end.

