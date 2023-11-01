unit PV_Unha;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-18
//HA by Harri Hirvola

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnha = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnha.Create(Str: TStream);
type THead = packed record
       Magic: array[0..1] of Char; // =HA
       TableCount: Word;
    end;
    TEntry = packed record
       TypeVer: Byte; //Ver<<4 | Type
       PackedSize: Cardinal;
       UnpackedSize: Cardinal;
       CRC32: Cardinal;
       DateTime: Cardinal;
       //Path: String;//null terminated
       //FName: String;//null terminated
       //ExtraLen: Byte;
       //Extra...
    end;

var Head: THead;
    Entry: TEntry;
    i: Integer;
    Path, FName: String;
    ExtraLen: Byte;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  try

    FStream.Read(Head, SizeOf(Head));

    for i:=0 to Head.TableCount-1 do begin

      FStream.Read(Entry, SizeOf(Entry));
      Path := ReadStrNull(FStream);
      FName := ReadStrNull(FStream);
      FStream.Read(ExtraLen, 1);

      if ExtraLen > 0 then FStream.Position := FStream.Position + ExtraLen;

      AFile.Name := FName;
      AFile.Offset := FStream.Position;
      AFile.PackedSize := Entry.PackedSize;
      AFile.UnpackedSize := Entry.UnpackedSize;
      AFile.PackMethod := pmStore;
      AFile.ModDate := Unix2DateTime(Entry.DateTime);
      AFile.CRC32 := Entry.CRC32;

      AddFile(AFile);

      FStream.Position := FStream.Position + Entry.PackedSize;
    end;
  except
  end;
end;

end.

