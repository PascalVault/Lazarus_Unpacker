# Lazarus_Unpacker
PV_Unpacker - simple pure Pascal library to unpack various archives (ZIP, RAR, LZMA, TAR...)

See also:
https://github.com/PascalVault/Lazarus_Packer

## Supported formats ##
- .ZIP, .JAR, .CBZ, .DOCX (store, deflate, lzma, bzip2)
- .RAR, .CBR (version 4 and 5; store)
- .TAR, .CBT
- .ARJ
- BlacHole .BH
- .BIG
- BGA .BZA/.GZA
- .CPIO
- .DPK
- .DRS
- .FTG
- .GZIP, .GZ
- .BZIP2, .BZ2
- .HA
- .LBR
- .LZH, .LHA
- .LZ, .LZMA
- .PCK
- Homm3 .LOD
- Quake .PAK
- Doom .WAD
- Quake .WAD
and more!

## Unsupported ###
- encrypted archives
- RAR files that use method different than "store"
- ARJ that use method different than "store"
- LZH that use method different than "store" and "lh1"

## Usage ###
    use PV_Unpacker;
    ...
    var Unp: TUnpacker;
    begin
      Unp := TUnpacker.Create(OpenDialog1.Filename);
    
      if Unp.GetFormat = '' then ShowMessage('Unsupported');
    
      for i:=0 to Unp.Count-1 do begin
        Memo1.Lines.Add( Unp.GetName(i) );
        if Unp.CanUnpack(i) then Unp.Extract(i, Unp.GetName(i) );
      end;
      Unp.Free;
    end;  
