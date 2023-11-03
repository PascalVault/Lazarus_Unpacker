unit about_box;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
  private

  public

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

end.

