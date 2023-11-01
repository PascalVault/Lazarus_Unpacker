unit PV_Unwad2;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-18
//Quake .WAD

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnwad2 = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnwad2.Create(Str: TStream);
type THead = packed record
      Magic: array[0..3] of Char;
      TableCount: Cardinal;
      TableOffset: Cardinal;
    end;
    TEntry = packed record
      Offset: Cardinal;
      PackSize: Cardinal;
      UnpackSize: Cardinal;
      Typee: Byte;
      Compression: Byte;
      Dummy: Word;
      FName: array[0..15] of Char;
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

    FStream.Position := Head.TableOffset;

    for i:=0 to Head.TableCount-1 do begin

      FStream.Read(Entry, SizeOf(Entry));

      AFile.Name := Entry.FName;
      AFile.Offset := Entry.Offset;
      AFile.PackedSize := Entry.PackSize;
      AFile.UnpackedSize := Entry.UnpackSize;

      if Entry.Compression = 0 then AFile.PackMethod := pmStore
      else                          AFile.PackMethod := pmOther;

      AFile.ModDate := 0;
      AFile.CRC32 := 0;

      AddFile(AFile);
    end;
  except
  end;
end;

end.

