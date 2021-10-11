unit PathScan;

interface

uses windows, sysutils, classes, filectrl, forms;

type
  TPathScanner = class(TObject)
  private
    fFirstDir   : string;
    fExtFilter  : string;
    fUseFilter  : boolean;
    fScanSubs   : boolean;
    fTotalDirs  : int64;
    fTotalFiles : int64;
    fTotalSize  : int64;
  protected
    procedure Scanner(Strings : TStrings; const CurDir : string);
    function AttrToStr(const Attr : integer) : string;
  public
    property Directory : string   read fFirstDir  write fFirstDir;
    property Filter    : string   read fExtFilter write fExtFilter;
    property UseFilter : boolean  read fUseFilter write fUseFilter;
    property ScanSubs  : boolean  read fScanSubs  write fScanSubs;
    property TotalDirs : int64    read fTotalDirs;
    property TotalFiles: int64    read fTotalFiles;
    property TotalSize : int64    read fTotalSize;
    procedure SelectDir;
    procedure Scan(Strings : TStrings);
    constructor Create;
    destructor Destroy; override;
  end;


implementation



constructor TPathScanner.Create;
begin
  inherited create;
  fTotalDirs := 0;
  fTotalFiles:= 0;
  fTotalSize := 0;
  fExtFilter := '';
  fFirstDir  := '';
  fUseFilter := false;
  fScanSubs  := false;
end;

destructor TPathScanner.Destroy;
begin
  inherited Destroy;
end;

procedure TPathScanner.Scan(Strings : TStrings);
begin
  fTotalDirs := 0;
  fTotalFiles:= 0;
  fTotalSize := 0;
  Strings.BeginUpdate;
  Scanner(Strings,fFirstDir);
  Strings.EndUpdate;
end;

function TPathScanner.AttrToStr(const Attr : integer) : string;
begin
  result := '......';
  if (attr and faVolumeId) <> 0 then  result[1] := 'V'
  else
  if (attr and faDirectory) <> 0 then result[1] := 'D'
  else
     result[1]  := 'F';

  if (attr and faArchive)  <> 0 then result[2] := 'A';
  if (attr and faHidden)   <> 0 then result[3] := 'H';
  if (attr and faReadOnly) <> 0 then result[4] := 'R';
  if (attr and faSysFile)  <> 0 then result[5] := 'S';
  if (attr and faSymLink)  <> 0 then result[6] := 'L';
end;

procedure TPathScanner.SelectDir;
var DResult : string;
begin
  If SelectDirectory('Sélèctionnez un dossier :','',DResult) Then
     fFirstDir := IncludeTrailingBackSlash(DResult);
end;


procedure TPathScanner.Scanner(Strings : TStrings; const CurDir : string);
var SRC : TSearchrec;
    SDN : string;
    LC  : integer;  { LC compteur pour processmessage | IDS index de l'element ajouté }
begin
  if DirectoryExists(CurDir) then begin
     { init du compteur a 0 }
     LC  := 0;
     SDN := copy(CurDir,length(fFirstDir)+1,length(CurDir));
     try
       { on recherche le premier fichier ou repertoire }
       if findfirst(CurDir+'*.*',faAnyFile,SRC) = 0 then begin
          { entrée dans la boucle }
          repeat
            { on incremente le compteur }
            inc(LC);

            { si le nom est different de . ou .. (root directory) }
            if (SRC.Name <> '.') and (SRC.Name <> '..') then begin

               { si l'attributs nous indique qu'il s'agit d'un repertoire }
               if fScanSubs and ((SRC.Attr and faDirectory) <> 0) then begin
                  { on recursse ScanProject sur le nouveau repertoire }
                  inc(fTotalDirs);
                  Scanner(Strings,CurDir+SRC.Name+'\');
               end else

               { si l'attribut nous indique qu'il s'agit d'un fichier }
               if not ((SRC.Attr and faVolumeID) <> 0) then begin
                  { on ajoute l'element et on recupere l'index dans IDS }
                  if (not fUseFilter) or (fUseFilter and (ExtractFileExt(SRC.Name) = fExtFilter)) then begin
                     inc(fTotalFiles);
                     inc(fTotalSize,SRC.Size);
                     Strings.Add(SDN+SRC.Name{+'>'+AttrToStr(SRC.Attr)+'>'+IntToStr(SRC.Size)});
                  end;
               end;
            end;
            { tout les dix passages on appel application.processmessage pour rafraichir
              l'affichage }
            if (LC mod 10) = 0 then begin
               application.ProcessMessages;
            end;

          { la boucle se termine quand FindNext ne trouve plus rien }
          until findnext(SRC) <> 0;
       end;
     finally
       { et enfin on ferme SRC }
       FindClose(SRC);
     end;
  end;
end;

end.
