TSynRestDataset
===============

By *EMartin* (Esteban Martin).


# Presentation

Migrating from *RemObjects* to *mORMot* I had to implement a GUI functionality that *RemObjects* has, an editable dataset connected through URL (RO version 3 
use SOAP and other components adapters, etc.).

My implementation is basic and the most probably is not the best, but works for me, the same use RESTful URL for get and update data, also get data from a 
*mORMot* interface based services returning a *mORMot* JSON array but cannot update because the table not exists in the database.

In this folder there are two units: `SynRestVCL.pas` and `SynRestMidasVCL.pas`, both have some duplicated code from its counterpart 
(`SynDBVCL.pas` and `SynDBMidasVCL.pas`) and the others, but the rest are modifications with use of RESTful instead of the `TSQLDBConnection` 
(this require the database client installed in the client machine).

A `TSQLModel` is required because the `TSynRestDataset` get the fields definition column type and size from this. Also is used from the `TSQLRecord` the 
defined validations (I used `InternalDefineModel`).

This was developed with Delphi 7 on Windows 7 and probably (almost sure) is not cross platform.

If this serves for others may be the best option will be that *ab* integrate this in the framework and make this code more *mORMot*. Meanwhile I will update 
here on github.

I hope this is helpful to someone.

# Contributors
- Houdw2006
- Logeas Informatique

# Examples
## Example 1: from a table

	// defining the table (excerpt from SampleData.pas)
    TSQLBiolife = class(TSQLRecord)
    protected
      fSpecies_No: TID;
	  fCategory: RawUTF8;
	  fCommon_Name: RawUTF8;
	  fSpecies_Name: RawUTF8;
	  fLength_cm: double;
	  fLength_in: double;
	  fNotes: RawUTF8;
	  fGraphic: TSQLRawBlob;
	  fSom: TSQLRawBlob;
	  class procedure InternalDefineModel(Props: TSQLRecordProperties); override;
	published
	  property Species_No: TID read fSpecies_No write fSpecies_No;
	  property Category: RawUTF8 index 15 read fCategory write fCategory;
	  property Common_Name: RawUTF8 index 30 read fCommon_Name write fCommon_Name;
	  property Species_Name: RawUTF8 index 40 read fSpecies_Name write fSpecies_Name;
	  property Length_cm: Double read fLength_Cm write fLength_Cm;
	  property Length_In: Double read fLength_In write fLength_In;
	  property Notes: RawUTF8 index 32767 read fNotes write fNotes;
	  property Graphic: TSQLRawBlob read fGraphic write fGraphic;
	  property Som: TSQLRawBlob read fSom write fSom;
	  property Modified: TModTime read fModified write fModified;
	end;

    ...

    { TSQLBioLife }

    class procedure TSQLBiolife.InternalDefineModel(Props: TSQLRecordProperties);
    begin
	  inherited;
	  AddFilterNotVoidText(['Category']);
	  AddFilterNotVoidText(['Common_Name']);
    end;

    // client (excerpt from fFactWin.pas)
    type
	  TForm1 = class(TForm)
		Panel1: TPanel;
		DBMemo1: TDBMemo;
		DataSource1: TDataSource;
		DBGrid1: TDBGrid;
		dbnvgr1: TDBNavigator;
		btnUpload: TButton;
		dlgOpenPic1: TOpenPictureDialog;
		img: TImage;
		DBLabel1: TDBText;
		pnl1: TPanel;
		lbl1: TLabel;
		dbtxtCommon_Name: TDBText;
		procedure FormCreate(Sender: TObject);
		procedure dbnvgr1Click(Sender: TObject; Button: TNavigateBtn);
		procedure btnUploadClick(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
	  private
		{ Private declarations }
		fImageLoader: TImageLoader;
		fServer, fPort, fRoot: RawUTF8;
		fSQLModel: TSQLModel;
		fRestClient: TSQLRestClientURI;
		SynRestDataset: TSynRestDataset;

		procedure DoOnAfterScroll(Dataset: TDataset);
	  public
		{ Public declarations }
	  end;

    ...

	procedure TForm1.FormCreate(Sender: TObject);
	var
	  I: Integer;
	begin
	  fImageLoader := TImageLoader.Create;	// used to load image from blob fields

	  fServer := 'LocalHost';
	  fPort := '8080';
	  fRoot := 'root';
	  fSQLModel := CreateSampleModel(fRoot);
	  fRestClient := TSQLHttpClient.Create(fServer, fPort, fSQLModel);

	  SynRestDataset := TSynRestDataset.Create(Nil);
	  SynRestDataset.RestClient := fRestClient;
	  SynRestDataset.CommandText := 'SELECT Species_No, Category, Common_Name, Species_Name, Length_cm, Length_in, Notes, '
		  + 'Modified, Graphic, Som FROM BioLife ORDER BY Species_No';
	  // calculated field
	  with TStringField.Create(SynRestDataset) do begin
		FieldName := 'Full_Name';
		Calculated:= True;
		Size := 50;
		Dataset := SynRestDataset;
	  end;
	  SynRestDataset.OnCalcFields := DoOnCalcFields;

	  // WHERE and/or ORDER BY clauses, and Parameters can be used as well.
	  //SynRestDataset.CommandText := 'SELECT * FROM BioLife '
	  //    + 'WHERE Species_No < :Species_No '
	  //    + 'ORDER BY Species_No ';
	  //SynRestDataset.Params.ParamByName('SPecies_No').Value := 100;
	  SynRestDataset.AfterScroll := DoOnAfterScroll;
	  SynRestDataset.Open;
	  // set display format in TTimeLogField
	  TTimeLogField(SynRestDataset.FieldByName('Modified')).DisplayFormat := 'YYYY-MM-DD HH:NN:SS';

	  DataSource1.DataSet := SynRestDataset;
	  // show the first record image
	  DoOnAfterScroll(Nil);
	  // hide blob and ID fields in the grid
	  for I := 0 to DBGrid1.Columns.Count-1 do
	  begin
		if (DBGrid1.Columns[I].Field.DataType = DB.ftBlob) then
		  DBGrid1.Columns[I].Visible := False
		else if (DBGrid1.Columns[I].Field.FieldName = 'ID') then  // Hide the ID column
		  DBGrid1.Columns[I].Visible := False;
	  end;
	end;


## Example 2: from a service 

    // defining the table, the service name and operation name are required
    TSQLRecordServiceName_OperationName = class(TSQLRecord)
    private
      fText: RawUTF8;
    published
      property Text: RawUTF8 index 255 read fText write fText;
    end;

    ...

    // server side implementation

    TServiceName =class(TInterfacedObjectWithCustomCreate, IServiceName)
    public
      ...
      // this function can also be function OperationName(const aParamName: RawUTF8): RawUTF8;
      function OperationName(const aParamName: RawUTF8; out aData: RawUTF8): Integer;
      ...
    end;

    ...

    function TServiceName.OperationName(const aParamName: RawUTF8; out aData: RawUTF8): Integer;
    begin
       Result := OK;
       aData := '[{"text":"test"},{"text":"test1"}]';    
    end;

    ...

    // client
    type
      TForm1 = class(TForm)
        DBGrid1: TDBGrid;
        DBNavigator1: TDBNavigator;
        btnOpen: TButton;
        edtURL: TEdit;
        dsRest: TDataSource;
        procedure FormCreate(Sender: TObject);
        procedure btnOpenClick(Sender: TObject);
        procedure DBNavigator1Click(Sender: TObject; Button: TNavigateBtn);
      private
        { Private declarations }
		fModel: TSQLModel;
        fRestDS: TSynRestDataset;
		fRestClient: TSQLRestClientURI;
      public
        { Public declarations }
      end;

    ...

    procedure TForm1.FormCreate(Sender: TObject);
    begin
	  fModel := TSQLModel.Create([TSQLRecordServiceName_OperationName], 'root');
	  fRestClient := TSQLHttpClient.Create(host, port, fModel);
      fRestDS := TSynRestDataset.Create(Self);
	  fRestDS.RestClient := fRestClient;
      dsRest.Dataset := fRestDS;
    end;

    procedure TForm1.btnOpenClick(Sender: TObject);
    begin
      fRestDS.Close;
      fRestDS.CommandText := edtURL.Text; // edtURL.Text = 'ServiceName.OperationName?aParamName=''XXX'''
      fRestDS.Open;
      // you can filter by named parameter:
      // fRestDS.CommandText := edtURL.Text; // 'ServiceName.OperationName?aParamName=:aParamName
      // fRestDS.Params.ParamByName('aParamName').Value := 'XXX'
      // fRestDS.Open;
    end;

# Forum Thread

See http://synopse.info/forum/viewtopic.php?id=2712

# License

Feel free to use and/or append to Lib and extend if needed.

===========================================================
# Previous modification by Dewen HOU (houdw2006) 2016-06-10

I enjoy the RESTful way to access the background database. After some discussion with EMartin in the forum of Synopse, I made some modification to TSynRestDataset and intended for a more mORMot way.

A RESTful instance (fRestClient: TSQLRest) is injected into classes TSynRestDataSet and TSynRestSQLDataSet, and which is used to handle all the background data CRUD.

The URI root is associated with the Data Model, and the IP and Port of the remote RESTful server are associated with the RestClient. The `TSQLModel` is not needed anymore because the `TSynRestDataset` can get the TSQLModel instance from the RestClient.

Access the background data by calling Service is not supported in this modification anymore, as I think according to the Single Responsibility Principle, this could be done by another class.

Now, the CommandText of TSynRestDataSet, which is inherrited from TClientDataset, is in the Normal SQL statement, as shown in the following code snippet:

				procedure TForm1.FormCreate(Sender: TObject);
				var
				  I: Integer;
				begin
				  fImageLoader := TImageLoader.Create;	// used to load image from blob fields

				  fServer := 'LocalHost';
				  fPort := '8080';
				  fRoot := 'root';
				  fSQLModel := CreateSampleModel(fRoot);
				  fRestClient := TSQLHttpClient.Create(fServer, fPort, fSQLModel);
				
				  SynRestDataset := TSynRestDataset.Create(Nil);
				  SynRestDataset.RestClient := fRestClient;
				  SynRestDataset.CommandText := 'SELECT * FROM BioLife '
				      + 'ORDER BY Species_No ';
				
				  // WHERE and/or ORDER BY clauses, and Parameters can be used as well.
				  //SynRestDataset.CommandText := 'SELECT * FROM BioLife '
				  //    + 'WHERE Species_No < :Species_No '
				  //    + 'ORDER BY Species_No ';
				  //SynRestDataset.Params.ParamByName('SPecies_No').Value := 100;
				  SynRestDataset.AfterScroll := DoOnAfterScroll;
				  SynRestDataset.Open;
				
				  DataSource1.DataSet := SynRestDataset;
				  // show the first record image
				  DoOnAfterScroll(Nil);
				  // hide blob and ID fields in the grid
				  for I := 0 to DBGrid1.Columns.Count-1 do
				  begin
				    if (DBGrid1.Columns[I].Field.DataType = DB.ftBlob) then
				      DBGrid1.Columns[I].Visible := False
				    else if (DBGrid1.Columns[I].Field.FieldName = 'ID') then  // Hide the ID column
				      DBGrid1.Columns[I].Visible := False;
				  end;
				end;


For the convinient of loading image into the TImage component, I have implemented a helpler class TImageLoader in unit ImageLoader.pas, which can be used to load JPeg, Png, Gif, Bmp image from the stream into TPicture object under XE8.

