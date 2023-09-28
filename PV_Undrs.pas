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
begin
  FStream := Str;

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;

    FStream.Read(Head, SizeOf(Head));

    SetLength(Groups, Head.NumberOfGroups);

    for j:=0 to Head.NumberOfGroups-1 do begin
      FStream.Read(Groups[j], Sizeof(Groups[j]));
    end;

    for j:=0 to Head.NumberOfGroups-1 do begin
      for i:=0 to Groups[j].TotalFiles-1 do begin

        FStream.Position := Groups[j].Offset;

        FStream.Read(Entry, SizeOf(Entry));

        if FCount = FSize then begin
          Inc(FSize, 1000);
          SetLength(FFiles, FSize);
        end;

        FFiles[FCount].Name := IntToStr(Entry.ID) + '.' + ReverseString(Trim(Groups[j].Ext));
        FFiles[FCount].Offset := Entry.Offset;
        FFiles[FCount].PackedSize := Entry.Size;
        FFiles[FCount].UnpackedSize := Entry.Size;
        FFiles[FCount].PackMethod := pmStore;

        Inc(FCount);
      end;
    end;
  except
  end;
end;

end.

