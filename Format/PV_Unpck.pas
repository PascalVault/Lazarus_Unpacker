unit PV_Unpck;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-18
//In the Groove .PCK

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnpck = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;

implementation

{ TUntar }

constructor TUnpck.Create(Str: TStream);
type THead = packed record
       Magic: array[0..3] of Char; //PCKF
       Comment: array[0..127] of Char;
       TableCount: Cardinal;
    end;
    TEntry = packed record
       UnpackedSize: Cardinal;
       PackedSize: Cardinal;
       Offset: Cardinal;
       FNameLen: Cardinal;
       Compression: Cardinal; //0=store
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

    for i:=0 to Head.TableCount-1 do begin

      FStream.Read(Entry, SizeOf(Entry));

      SetLength(FName, Entry.FNameLen);
      FStream.Read(FName[1], Entry.FNameLen);

      AFile.Name := FName;
      AFile.Offset := Entry.Offset;
      AFile.PackedSize := Entry.PackedSize;
      AFile.UnpackedSize := Entry.UnpackedSize;

      if Entry.Compression = 0 then AFile.PackMethod := pmStore
      else                          AFile.PackMethod := pmDeflate; //was described as ZLIB but actually just deflate

      AFile.ModDate := 0;
      AFile.CRC32 := 0;

      AddFile(AFile);
    end;
  except
  end;
end;

end.

