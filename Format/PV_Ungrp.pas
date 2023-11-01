unit PV_Ungrp;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-15
//Duke Nukem 3D .GRP

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUngrp = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUngrp.Create(Str: TStream);
type THead = packed record
      Magic: array[0..11] of Char; //"KenSilverman"
      Count: Cardinal;
    end;
    TEntry = packed record
      FName: array[0..11] of Char;
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

    for i:=0 to Head.Count-1 do begin

      FStream.Read(Entry, SizeOf(Entry));

      AFile.Name := Entry.FName;
      AFile.Offset := 0;
      AFile.PackedSize := Entry.Size;
      AFile.UnpackedSize := Entry.Size;
      AFile.PackMethod := pmStore;
      AFile.ModDate := 0;
      AFile.CRC32 := 0;

      AddFile(AFile);
    end;

    for i:=0 to Head.Count-1 do begin
      FFiles[i].Offset := FStream.Position;

      FStream.Position := FStream.Position + FFiles[i].UnpackedSize;
    end;

  except
  end;
end;

end.

