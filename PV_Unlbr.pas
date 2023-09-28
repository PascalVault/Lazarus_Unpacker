unit PV_Unlbr;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-18
//.LBR by Gary P. Novosielski

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

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
begin
  FStream := Str;

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;

    while True do begin

      FStream.Read(Entry, SizeOf(Entry));

      if FCount = FSize then begin
        Inc(FSize, 1000);
        SetLength(FFiles, FSize);
      end;

      if Entry.Size = 0 then break;
      if Entry.Offset = 0 then continue; //skip first entry- it's the main directory

      FFiles[FCount].Name := Entry.FName + '.' + Entry.Ext;
      FFiles[FCount].Offset := Entry.Offset * 128;
      FFiles[FCount].PackedSize := Entry.Size * 128;
      FFiles[FCount].UnpackedSize := Entry.Size * 128;
      FFiles[FCount].PackMethod := pmStore;

      Inc(FCount);

      if FStream.Position > FStream.Size-1 then break;
    end;
  except
  end;
end;

end.

