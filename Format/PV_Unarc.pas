unit PV_Unarc;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-21
//Thom Henderson .ARC

interface

uses
  Classes, SysUtils, PV_Unpack, CRC16_ARC, Dialogs;

  { TUnPak }

type
  TUnarc = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

//Methods:
//0 - EOF
//1 - unpacked (obsolete)
//2 - unpacked
//3 - RLE
//4 - squeezed (after packing)
//5 - crunched (obsolete)
//6 - crunched (after packing) (obsolete)
//7 - crunched (after packing, using faster hash algorithm)
//8 - crunched (after packing, using dynamic LZW variations)
//9 - Squashed c/o Phil Katz (no packing)

constructor TUnarc.Create(Str: TStream);
type THead = packed record
       Magic: Byte; //1A
       Method: Byte;
     end;
     TEntry = packed record
       FName: array[0..12] of Char;    //max 12 bytes+null
       PackedSize: Cardinal;
       FileDate: Word;
       FileTime: Word;
       CRC16: Word;
       UnpackedSize: Cardinal;
//total 29 bytes
    end;

var Entry: TEntry;
    Head: THead;
    i: Integer;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;
  FHasherClass := THasherCRC16_ARC;

  try
    while FStream.Position < FStream.Size do begin

      FStream.Read(Head, SizeOf(Head));

      if (Head.Magic <> $1A) or (Head.Method = 0) then break;

      FStream.Read(Entry, SizeOf(Entry));

      AFile.Name := Entry.FName;// + '      [[' + inttostr(head.method);
      AFile.Offset := FStream.Position;
      AFile.PackedSize := Entry.PackedSize;
      AFile.UnpackedSize := Entry.UnpackedSize;

      if Head.Method = 2 then      AFile.PackMethod := pmStore
      else if Head.Method = 3 then AFile.PackMethod := pmRLE90
      else if Head.Method = 4 then AFile.PackMethod := pmSqueeze
      else if Head.Method = 8 then AFile.PackMethod := pmCrunch8
      else if Head.Method = 9 then AFile.PackMethod := pmSquash
      else                         AFile.PackMethod := pmOther;

//      if not (head.method in [2,8,9]) then
 //     showmessage(inttostr(head.method));

      AFile.ModDate := Dos2DateTime(Entry.FileTime, Entry.FileDate);
      AFile.CRC32 := Entry.CRC16;

      AddFile(AFile);

      FStream.Position := FStream.Position + Entry.PackedSize;
    end;

  except
  end;
end;

end.

