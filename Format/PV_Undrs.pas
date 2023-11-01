unit PV_Undrs;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-18
//Age of Empires .DRS

interface

uses
  Classes, SysUtils, StrUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUndrs = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUndrs.Create(Str: TStream);
type THead = packed record
       Copyright: array[0..35] of Char;
       Version: Cardinal;
       Unknown: array[0..15] of Char;
       NumberOfGroups: Cardinal;
       Offset: Cardinal;
    end;
    TGroup = packed record
       Ext: array[0..3] of Char; //space terminated
       Offset: Cardinal;
       TotalFiles: Cardinal;
    end;
    TEntry = packed record
       ID: Cardinal;
       Offset: Cardinal;
       Size: Cardinal;
    end;

var Head: THead;
    Entry: TEntry;
    Groups: array of TGroup;
    i,j: Integer;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  try
    FStream.Read(Head, SizeOf(Head));

    SetLength(Groups, Head.NumberOfGroups);

    for j:=0 to Head.NumberOfGroups-1 do begin
      FStream.Read(Groups[j], Sizeof(Groups[j]));
    end;

    for j:=0 to Head.NumberOfGroups-1 do begin
      for i:=0 to Groups[j].TotalFiles-1 do begin

        FStream.Position := Groups[j].Offset;

        FStream.Read(Entry, SizeOf(Entry));

        AFile.Name := IntToStr(Entry.ID) + '.' + ReverseString(Trim(Groups[j].Ext));
        AFile.Offset := Entry.Offset;
        AFile.PackedSize := Entry.Size;
        AFile.UnpackedSize := Entry.Size;
        AFile.PackMethod := pmStore;
        AFile.ModDate := 0;
        AFile.CRC32 := 0;

        AddFile(AFile);
      end;
    end;
  except
  end;
end;

end.

