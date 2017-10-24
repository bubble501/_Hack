////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   EditorCtor.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-2
//  Comment     :
//
//
////////////////////////////////////////////////////////////////////////////////

unit EditorCtor;

{����ʵ�ֵ�IssnEditor��TssnEditor��TssnMemoEditor��TssnWorkSpace��TssnWorkSpaceMgr��TssnTabWorkSpace��TssnTabWorkSpaceMgr�����һ��
  �Ѿ��ǿ������е�һ����ϵͳ��
  ��ΪSunny SmartNote��Դ�汾�ĺ��Ĺ��ܲ��ֵ������ϵͳ���Ѿ���������ˣ�ֻ����Щ�໹û�б�ʵ�ʴ�����ʵ���������
  }
{Ҫ��������Ķ��󣬵�Ȼ�Ǻ����׵ġ�
  ֻ�ǣ�����Sunny SmartNote֧�ֶ��ֱ༭��������л�����TssnEditor��ͬ��������л���
  ֧�ֶ��ֹ�������������л�����TssnWorkSpace/TTssnWorkSpaceMgr��ͬ��������л���
  �򵥵ڴ�����Щ��Ķ����Ǵﲻ����������Ե�Ŀ�ĵ�
  }
{��ˣ���������辶����Ѱ����������ԡ�Sunny SmartNote�ҵ��˽�������������ʹ�ù�����TssnEditorCtor/TssnWorkSpaceMgrCtor
  }
{�����У�Ҫ��༭������ǿ��滻�ģ��๤�����Ľ�����Ҳ�ǿ��滻��
  ���Ҫ����ͬTssnEditor��TssnWorkSpace/TssnWorkSpaceMgr��������ʵ���Ĵ������������
  �������������ָ�������������͡����ַ���ʵ������
  }
{������Ҫ����Եĵط�������Ҫ����
  �ǵģ������ǶԴ������ʵ�����߼����г����ʱ����
  ��ʱ������Ҫ������������
  ��������������һ��������ֻ��Ҫ��������Ҫʲô���͵Ĳ�Ʒ��������ʵ��������������Ϊ��������
  }
{���ȣ���ҪΪĳһ�֡�������������һ���ӿڹ淶���������ࡣ
  Ȼ��Ӹó�������������������ĳһ���Ͳ�Ʒ��������
  ����������Ҫ������ͬ�Ĳ�Ʒʱ��ֻҪ��������ͬ�ġ���������ʵ�����󣬾Ϳ�����������ͬ�ġ���Ʒ��
  }

interface

uses stdctrls, controls,
     Editor;

type
    {�����༭������ġ���������
      ����һ�������࣬��Ϊ�����������ࣨ����Ĺ�����������һ��ͳһ�淶
      ���������Ӧ��ÿһ��TssnEditor�������࣬��Ϊ���ǵ�������

      �������Ķ���ӿںܼ򵥣�һ��ֻ��Ҫһ��������Ʒʵ���ķ�������
        ����TssnEditorCtor����Ψһ��һ��������CreateAnEditor
        �÷���ͨ������var Editor : TssnEditor ����������������TssnEditor���͵Ķ�������
      ���԰�CreateAnEditor����Ϊһ�������鷽�����ɾ��崴��ÿ�ַ��ı༭��ʵ����TssnEditorCtor����������ʵ��
        ���ǣ���һЩ������Ҫ���ڻ��������
        ��˻���ʹ���Ѿ�ʹ�ù���ε�Template Method������
        ��CreateAnEditor����Ϊ���鷽������Ϊ�����׼�����һ��DoCreateAnEditor�ĳ����鷽����������������ʵ��
      }
    TssnEditorCtor = class
    protected
        function DoCreateAnEditor(var Editor : TssnEditor; ParentCtrl : TWinControl) : Integer; virtual; abstract;
    public
        function CreateAnEditor(var Editor : TssnEditor; ParentCtrl : TWinControl) : Integer;
    end;

    {TssnMemoEditorCtor��TssnMemoEditor�Ĺ�����
      }
    TssnMemoEditorCtor = class(TssnEditorCtor)
    protected
        function DoCreateAnEditor(var Editor : TssnEditor; ParentCtrl : TWinControl) : Integer; override;
    end;

    {TssnRichEditorCtor��TssnRichEditor�Ĺ�����
      }
    TssnRichEditorCtor = class(TssnEditorCtor)
    protected
        function DoCreateAnEditor(var Editor : TssnEditor; ParentCtrl : TWinControl) : Integer; override;
    end;

implementation

uses MemoEditor, RichEditor, SettingMgr, GlobalObject;

{ tssnMemoEdtitorCtor }

{����һ��TssnMemoEditorʵ��
  }
function TssnMemoEditorCtor.DoCreateAnEditor(var Editor: TssnEditor; ParentCtrl : TWinControl): Integer;
begin
    Editor := TssnMemoEditor.Create(ParentCtrl);
    Result := Integer(Editor <> nil);
end;

{ TssnEditorCtor }

{��TssnEditorCtor��Ҫʵ�ֵģ�����ֻ��CreateAnEditor
  �÷������㷨�ǵ��ñ��������า�ǵĳ����鷽��DoCreateAnEditor��Ȼ��Ϊ���������Ķ�������һЩ��ʼֵ
  }
function TssnEditorCtor.CreateAnEditor(var Editor: TssnEditor;
  ParentCtrl: TWinControl): Integer;
begin
    {����ParentCtrlָ���༭��������ڸ��������
      �����������DoCreateAnEditor�Դ��������ı༭�������ʵ��
      ����ı༭�������������ʹ������TssnEditorCtor��������ʵ��
      }
    Result := DoCreateAnEditor(Editor, ParentCtrl);

    {�����ɹ�����Ĭ���������������ڱ༭�����
      }
    if Boolean(Result) then
        Editor.SetFont(g_SettingMgr.GetDefaultFont());
end;

{ TssnRichEditorCtor }

{����һ��TssnRichEditorʵ��
  }
function TssnRichEditorCtor.DoCreateAnEditor(var Editor: TssnEditor;
  ParentCtrl: TWinControl): Integer;
begin
    Editor := TssnRichEditor.Create(ParentCtrl);
    Result := Integer(Editor <> nil);
end;

end.
