unit PV_Unepf;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-15
//The Lion King .EPF

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnepf = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnepf.Create(Str: TStream);
type THead = packed record
      Magic: array[0..3] of Char; //"EPFS"
      Offset: Cardinal;
      Unknown: Byte; 
      Count: Word;
    end;
    TEntry = packed record
      FName: array[0..12] of Char;
      Compression: Byte; //0=store
      PackedSize: Cardinal;  
      UnpackedSize: Cardinal;
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

    FStream.Position := Head.Offset;

    for i:=0 to Head.Count-1 do begin

      FStream.Read(Entry, SizeOf(Entry));

      AFile.Name := Entry.FName;
      AFile.Offset := FStream.Position;
      AFile.PackedSize := Entry.PackedSize;
      AFile.UnpackedSize := Entry.UnpackedSize;
      AFile.PackMethod := pmStore;
      AFile.ModDate := 0;
      AFile.CRC32 := 0;

      AddFile(AFile);
    end;

  except
  end;
end;

end.

