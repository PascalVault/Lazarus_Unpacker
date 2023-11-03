program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unit1, preview_box, about_box, test_box, info_box
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TPreviewForm, PreviewForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TTestForm, TestForm);
  Application.CreateForm(TInfoForm, InfoForm);
  Application.Run;
end.

