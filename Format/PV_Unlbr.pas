unit PV_Unlbr;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-18
//.LBR by Gary P. Novosielski

interface

uses
  Classes, SysUtils, PV_Unpack, CRC16_XMODEM, Dialogs;

  { TUnPak }

type
  TUnlbr = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnlbr.Create(Str: TStream);
type TEntry = packed record
       Status: Byte;
       FName: array[0..7] of Char;
       Ext: array[0..2] of Char;
       Offset: Word;
       Size: Word;
       CRC16: Word;
       CreatDate: Word;
       ModDate: Word;
       CreatTime: Word;
       ModTime: Word;
       Padding: Byte;
       Filler: array[0..4] of Byte;
    end;

var Entry: TEntry;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;
  FHasherClass := THasherCRC16_XMODEM;

  try
    while True do begin

      FStream.Read(Entry, SizeOf(Entry));

      if Entry.Size = 0 then break;
      if Entry.Offset = 0 then continue; //skip first entry- it's the main directory

      AFile.Name := Entry.FName + '.' + Entry.Ext;
      AFile.Offset := Entry.Offset * 128;
      AFile.PackedSize := Entry.Size * 128;
      AFile.UnpackedSize := Entry.Size * 128;
      AFile.PackMethod := pmStore;

      AFile.ModDate := Dos2DateTime(Entry.CreatTime, Entry.CreatDate);   //a lot of LBR files have zeroes
      AFile.CRC32 := Entry.CRC16;

      AddFile(AFile);

      if FStream.Position > FStream.Size-1 then break;
    end;
  except
  end;
end;

end.

