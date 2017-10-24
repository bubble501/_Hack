////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   IntfEditor.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-13
//  Comment     :   Interface of Editor
//
//
////////////////////////////////////////////////////////////////////////////////

unit IntfEditor;

interface

uses Graphics, Classes, Dialogs;

type
    TssnWordCountRec = record
        AnsiChar : Integer;
        MultiChar : Integer;
        NumChar : Integer;
        Other : Integer;
    end;

    {TssnWorkSpace������TssnEditor�Ĺ��ܣ�Ϊ�˱�֤���͸�TssnWorkSapce����༭������йص������ܹ���ȷ��ת�͸�TssnEditor����
      ���Ǳ���ӵ��һ����ͬ�Ĺ����Ӽ����������߼̳�һ����ͬ�Ļ���IssnEditor

      IssnEditor��һ���ӿڣ������ࣩ�����涨���������������ʵ����Щ���ܡ���TssnWorkSpace��TssnEditor����������ʱ��TssnWorkSpace
        �ͻ��о��Եġ����š������ӵ�������ת�������ڲ���TssnEditorʵ��
      ����IssnEditorֻ��Ҫ����ӿڣ�����Ҫʵ��
      }
    IssnEditor = class
    public
        function GetFileName() : String; virtual; abstract;        //��ȡ����������Ӧ���ļ���
        function GetSaved() : Boolean; virtual; abstract;          //��ȡ�������Ƿ��ڡ��ѱ��桱״̬
        function Save() : Boolean; virtual; abstract;              //�Ե�ǰ�ļ������湤�����ڵ��ı����ļ�
        function SaveAs() : Boolean; virtual; abstract;            //���Ϊ
        function GetSelectText() : String; virtual; abstract;      //��ȡ����������ѡ�е��ַ���
        procedure SetFont(Font : TFont); virtual; abstract;        //���ù�������ʾ����
        procedure Undo(); virtual; abstract;                       //ִ�С�����������
        function CanUndo() : Boolean; virtual; abstract;           //�������Ƿ�������ִ�С�����������״̬
        procedure Redo(); virtual; abstract;                       //ִ�С�����������
        function CanRedo() : Boolean; virtual; abstract;           //�������Ƿ�������ִ�С�����������״̬
        procedure Cut(); virtual; abstract;                        //ִ�С����С�����
        function CanCut() : Boolean; virtual; abstract;            //�������Ƿ�������ִ�С����С�����״̬
        procedure Copy(); virtual; abstract;                       //ִ�С����ơ�����
        function CanCopy() : Boolean; virtual; abstract;           //�������Ƿ�������ִ�С����ơ�����״̬
        procedure Paste(); virtual; abstract;                      //ִ�С�ճ��������
        function CanPaste() : Boolean; virtual; abstract;          //�������Ƿ�������ִ�С�ճ��������״̬
        procedure DeleteSelection(); virtual; abstract;            //ִ�С�ɾ����ѡ�ַ�������
        function CanDeleteSelection() : Boolean; virtual; abstract;//�������Ƿ�������ִ�С�ɾ����ѡ�ַ�������״̬
        procedure DeleteLine(); virtual; abstract;                 //ִ�С�ɾ��һ�С�����
        procedure SelectAll(); virtual; abstract;                  //ִ�С�ȫѡ������
        function FindNext(Text : String; Option : TFindOptions) : Boolean; virtual; abstract;                //������һ��
        function Replace(FindText, ReplaceText : String; Option : TFindOptions) : Integer; virtual; abstract;//�滻
        function GetWordCount() : TssnWordCountRec; virtual; abstract;  //ͳ�Ƶ�ǰ����������
        function GetWordWrap() : Boolean; virtual; abstract;            //��ȡ��ǰ�������Ƿ��ڡ��Զ����С�״̬
        procedure SetWordWrap(WordWrap : Boolean); virtual; abstract;   //���á��Զ����С�
    end;

implementation

end.
