function varargout = RelapGUI(varargin)

    % RELAPGUI MATLAB code for RelapGUI.fig
    %      RELAPGUI, by itself, creates a new RELAPGUI or raises the existing
    %      singleton*.
    %
    %      H = RELAPGUI returns the handle to a new RELAPGUI or the handle to
    %      the existing singleton*.
    %
    %      RELAPGUI('CALLBACK',hObject,eventData,handles,...) calls the local
    %      function named CALLBACK in RELAPGUI.M with the given input arguments.
    %
    %      RELAPGUI('Property','Value',...) creates a new RELAPGUI or raises the
    %      existing singleton*.  Starting from the left, property value pairs are
    %      applied to the GUI before RelapGUI_OpeningFcn gets called.  An
    %      unrecognized property name or invalid value makes property application
    %      stop.  All inputs are passed to RelapGUI_OpeningFcn via varargin.
    %
    %      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
    %      instance to run (singleton)".
    %
    % See also: GUIDE, GUIDATA, GUIHANDLES

    % Edit the above text to modify the response to help RelapGUI

    % Last Modified by GUIDE v2.5 10-Mar-2019 20:49:40

    % Begin initialization code - DO NOT EDIT
    gui_Singleton = 1;
    gui_State = struct('gui_Name',       mfilename, ...
                       'gui_Singleton',  gui_Singleton, ...
                       'gui_OpeningFcn', @RelapGUI_OpeningFcn, ...
                       'gui_OutputFcn',  @RelapGUI_OutputFcn, ...
                       'gui_LayoutFcn',  [] , ...
                       'gui_Callback',   []);
    if nargin && ischar(varargin{1})
        gui_State.gui_Callback = str2func(varargin{1});
    end

    if nargout
        [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
    else
        gui_mainfcn(gui_State, varargin{:});
    end
    % End initialization code - DO NOT EDIT

% --- Executes just before RelapGUI is made visible.
function RelapGUI_OpeningFcn(hObject, eventdata, handles, varargin)
    % This function has no output args, see OutputFcn.
    % hObject    handle to figure
    % eventdata  reserved - to be defined in a future version of MATLAB
    % handles    structure with handles and user data (see GUIDATA)
    % varargin   command line arguments to RelapGUI (see VARARGIN)

    % Choose default command line output for RelapGUI
    handles.output = hObject;
        
    %read default code directory
    try
        thisScriptPath=varargin{1};
        fid=fopen([thisScriptPath,'\RelapCodePath.txt']);
        relapCodePath = fgetl(fid);
        fclose(fid)
    catch
        relapCodePath = '0';
    end
    
    update2Flag=0;
    
    while ~exist([relapCodePath,'\Relap.exe'],'file')
        ~exist([relapCodePath,'\Relap.exe'],'file')
%         waitfor(msgbox('Relap5.exe not found. Please provide path to folder with Relap5.exe'));
        relapCodePath=uigetdir;
        ~exist([relapCodePath,'\Relap.exe'],'file')
        update2Flag=1;
    end
    
    if update2Flag
        fid = fopen([thisScriptPath,'\RelapCodePath.txt'],'w');
        fprintf(fid,'%s',relapCodePath);
        fclose(fid);
    end
    
    handles.dirCode=relapCodePath;
    
    %update handles structure
    guidata(hObject, handles)
    

    % UIWAIT makes RelapGUI wait for user response (see UIRESUME)
    % uiwait(handles.figure1);

% --- Outputs from this function are returned to the command line.
function varargout = RelapGUI_OutputFcn(hObject, eventdata, handles) 
    % varargout  cell array for returning output args (see VARARGOUT);
    % hObject    handle to figure
    % eventdata  reserved - to be defined in a future version of MATLAB
    % handles    structure with handles and user data (see GUIDATA)

    % Get default command line output from handles structure
    varargout{1} = handles.output;

% --- Executes on button press in genInput.
function genInput_Callback(hObject, eventdata, handles) %#ok<DEFNU>
    clc
    
    %get directory close to user wishes
    default_dir=get(handles.file_path,'String');
    
    %check if the input is manual or automatic (from experiment)
    input_type=get(handles.inputManual,'Value');
    
    %what about NC gases?
    NCmodelFlag=handles.disableNC_box.Value;
    
    %get calculation parameters
    handles.mindt=get(handles.mindtBox,'String');
    handles.initial_maxdt=get(handles.initial_maxdtBox,'String');
    handles.final_maxdt=get(handles.final_maxdtBox,'String');
    handles.initial_endtime=get(handles.initial_endtimeBox,'String');
    handles.endtime=get(handles.endtimeBox,'String');
    %take value specificed by user (in seconds), divide it by final dT (also in seconds) and what you get is a frequency
    %heep major restart and minor the same to keep data storage period
    %constant - otherwise you'll get multiple frequencies of storage
    %(with major, restart and minor being three frequencies)
    handles.minor_initial=num2str(str2double(get(handles.minorBox,'String'))/str2double(handles.initial_maxdt));
    handles.minor_final=num2str(str2double(get(handles.minorBox,'String'))/str2double(handles.final_maxdt));
    handles.major_initial=num2str(str2double(get(handles.minorBox,'String'))/str2double(handles.initial_maxdt));
    handles.major_final=num2str(str2double(get(handles.minorBox,'String'))/str2double(handles.final_maxdt));
    handles.restart_initial=handles.minor_initial;
    handles.restart_final=handles.minor_final;
    handles.initial_cond=get(handles.initial_condBox,'Value');
    handles.action_start=get(handles.actionstartBox,'String');
    handles.action_ramp=get(handles.rampBox,'String');
    handles.timestep_cntrl=get(handles.timestepcntrlBox,'String');
    handles.nodalizationType=get(handles.buttonSingletube,'Value');
    %feedback
    fbNo=handles.buttonFbno.Value;
    fb121to110=handles.buttonFb121to110.Value;
    if fbNo
        handles.feedback=1;
    elseif fb121to110
        handles.feedback=2;
    else
        handles.feedback=3;
    end

    %generate input decks
    tempDir=generateRelapInput_annulus_for_experiments(handles,input_type,default_dir,NCmodelFlag);
    
    %update directory if changed
%       default_dir=cell2mat(tempDir);
%         singPos=strfind(tempDir{1},'\');
%         default_dir=tempDir{1}(1:singPos(end));
%     default_dir=tempDir{1};
    handles.file_path.String=default_dir;
    
    %update handles structure
    guidata(hObject, handles)

% --- Executes on button press in runRelap.
function runRelap_Callback(hObject, eventdata, handles) %#ok<DEFNU>
    clc
    
    %get directory close to user wishes
    default_dir=get(handles.file_path,'String'); 
    %get RELAP runs options 
    starting_file=str2double(get(handles.startFile,'String'));
    starting_batch=str2double(get(handles.startBatch,'String'));
    execution_time=str2double(get(handles.execTime,'String'));
    batch_size=str2double(get(handles.batchSize,'String'));   
    %run RELAP
    runRelap(handles.dirCode,default_dir,starting_file,execution_time,starting_batch,batch_size,0,0)
    
    %update handles structure
    guidata(hObject, handles)
        
% --- Executes on button press in procResults.
function procResults_Callback(hObject, eventdata, handles) %#ok<DEFNU>
    clc
    %get directory close to user wishes
    default_dir=get(handles.file_path,'String');
    %process results
    processResults(default_dir,0,0);
    
    %update handles structure
    guidata(hObject, handles)
    
        
    % --- Executes on button press in plotResults. 
function plotResults_Callback(hObject, eventdata, handles) %#ok<DEFNU>
    clc
    %get directory close to user wishes
    default_dir=get(handles.file_path,'String');
    %start plotting
    tempDir=plotter_mat(default_dir,0,0);
        
    %update directory if changed
%             default_dir=cell2mat(tempDir);
    singPos=strfind(tempDir{1},'\');
    default_dir=tempDir{1}(1:singPos(end));
    handles.file_path.String=default_dir;
    %update handles structure
    guidata(hObject, handles)
    

% --- Executes on button press in sequence.
function sequence_Callback(hObject, eventdata, handles) %#ok<DEFNU>
    clc
    genFlag=get(handles.genInputBox,'Value');
    runFlag=get(handles.runRelapBox,'Value');
    ProcFlag=get(handles.procResultsBox,'Value');
    PlotFlag=get(handles.plotResultsBox,'Value');
    
    %check if there are no processing gaps
    processingString=[genFlag runFlag ProcFlag PlotFlag];
    er_1=[1 0 1];
    er_2=[1 0 0 1];
    gap1=strfind(processingString,er_1);
    
    if gap1==1
        button = questdlg('No gaps in processing change allowed. Continue with ''Run RELAP5'' option as ON?','Action required','Yes','No','Yes');
        if strcmp(button,'Yes')
            set(handles.runRelapBox,'Value',1)
            runFlag=1;
        else
            return
        end
        
    elseif gap1==2      
        button = questdlg('No gaps in processing change allowed. Continue with ''Process Results'' option as ON?','Action required','Yes','No','Yes');
        if strcmp(button,'Yes')
            set(handles.procResultsBox,'Value',1)
            ProcFlag=1;
        else
            return
        end
    else

        if isequal(processingString,er_2)
            button = questdlg('No gaps in processing change allowed. Continue with ''Run RELAP5'' and ''Process Results'' options as ON?','Action required','Yes','No','Yes');
            if strcmp(button,'Yes')
                set(handles.runRelapBox,'Value',1)
                set(handles.procResultsBox,'Value',1)
                runFlag=1;
                ProcFlag=1;
            else
                return
            end            
        end
    end
    
    %run required code parts
    %get directory close to user wishes
    default_dir=get(handles.file_path,'String');
    
    if genFlag  
        %check if the input is manual or automatic (from experiment)
        input_type=get(handles.inputManual,'Value');
        
         %get calculation parameters
        handles.mindt=get(handles.mindtBox,'String');
        handles.initial_maxdt=get(handles.initial_maxdtBox,'String');
        handles.final_maxdt=num2str(str2double(get(handles.final_maxdtBox,'String')));
        handles.initial_endtime=get(handles.initial_endtimeBox,'String');
        handles.endtime=get(handles.endtimeBox,'String');
        %take value specificed by user (in seconds), divide it by final dT (also in seconds) and what you get is a frequency
        %heep major restart and minor the same to keep data storage period
        %constant - otherwise you'll get multiple frequencies of storage
        %(with major, restart and minor being three frequencies)
        handles.minor_initial=num2str(str2double(get(handles.minorBox,'String'))/str2double(handles.initial_maxdt));
        handles.minor_final=num2str(str2double(get(handles.minorBox,'String'))/str2double(handles.final_maxdt));
        handles.major_initial=num2str(str2double(get(handles.minorBox,'String'))/str2double(handles.initial_maxdt));
        handles.major_final=num2str(str2double(get(handles.minorBox,'String'))/str2double(handles.final_maxdt));
        handles.restart_initial=handles.minor_initial;
        handles.restart_final=handles.minor_final;
        handles.initial_cond=get(handles.initial_condBox,'Value');
        handles.action_start=get(handles.actionstartBox,'String');
        handles.action_ramp=get(handles.rampBox,'String');
        handles.timestep_cntrl=get(handles.timestepcntrlBox,'String');
        handles.nodalizationType=get(handles.buttonSingletube,'Value');
        
        %feedback line
        fbNo=handles.buttonFbno.Value;
        fb121to110=handles.buttonFb121to110.Value;
        if fbNo
            handles.feedback=1;
        elseif fb121to110
            handles.feedback=2;
        else
            handles.feedback=3;
        end
    
        %generate input decks
        tempDir=generateRelapInput_annulus_for_experiments(handles,input_type,default_dir);
        
        %update directory if changed
%       default_dir=cell2mat(tempDir);
%         singPos=strfind(tempDir{1},'\');
%         default_dir=tempDir{1}(1:singPos(end));
        default_dir=tempDir{1};
        handles.file_path.String=default_dir;

        disp('-------------------------------------------')
        
    end
    
    %parameter firstAndSeq is a boolean showing the function that it's
    %being run in sequence mode AND being first to start - effectively
    %requiring user to pick a directory with files to process - following
    %code parts will employ the same directory
    if runFlag
        %check if this is the first element to be run
        if ~genFlag
            first=1;
        else
            first=0;
        end
        disp('Running Relap')
        %get parameters of handling RELAP runs   
        starting_file=str2double(get(handles.startFile,'String'));
        starting_batch=str2double(get(handles.startBatch,'String'));
        execution_time=str2double(get(handles.execTime,'String'));
        batch_size=str2double(get(handles.batchSize,'String'));
        %run RELAP
        tempDir=runRelap(handles.dirCode,default_dir,starting_file,execution_time,starting_batch,batch_size,1,first);
        
        %update directory if changed
        if first
%             default_dir=cell2mat(tempDir);
            singPos=strfind(tempDir{1},'\');
            default_dir=tempDir{1}(1:singPos(end));
            handles.file_path.String=default_dir;
        end
        disp('-------------------------------------------')
    end
    
    if ProcFlag
        if ~runFlag
            first=1;
        else
            first=0;
        end    
        disp('Processing Files')
        %process results
        tempDir=processResults(default_dir,1,first);
        
        %update directory if changed
        if first
%             default_dir=cell2mat(tempDir);
            singPos=strfind(tempDir{1},'\');
            default_dir=tempDir{1}(1:singPos(end));
            handles.file_path.String=default_dir;
        end
        
        disp('-------------------------------------------')
    end
    
    if PlotFlag
        if ~ProcFlag
            first=1;
        else
            first=0;
        end
        disp('Plotting files')
        %start plotting
        plotter_mat(default_dir,1,first);
        
        %update directory if changed
%         if first
% %             default_dir=cell2mat(tempDir);
%             singPos=strfind(tempDir{1},'\');
%             default_dir=tempDir{1}(1:singPos(end));
%             handles.file_path.String=default_dir;
%         end
        
        disp('-------------------------------------------')
    end
    disp('Sequence finished')
    
    %update handles structure
    guidata(hObject, handles)

% --- Executes on button press in genInputBox.
function genInputBox_Callback(hObject, eventdata, handles) %#ok<DEFNU>
 
% --- Executes on button press in runRelapBox.
function runRelapBox_Callback(hObject, eventdata, handles) %#ok<DEFNU>

% --- Executes on button press in procResultsBox.
function procResultsBox_Callback(hObject, eventdata, handles) %#ok<DEFNU>
    
% --- Executes on button press in plotResultsBox.
function plotResultsBox_Callback(hObject, eventdata, handles) %#ok<DEFNU>

function Pps_Callback(hObject, eventdata, handles) %#ok<DEFNU>

% --- Executes during object creation, after setting all properties.
function Pps_CreateFcn(hObject, eventdata, handles) %#ok<DEFNU>

    if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
        set(hObject,'BackgroundColor','white');
    end

function NC_Callback(hObject, eventdata, handles) %#ok<DEFNU>

% --- Executes during object creation, after setting all properties.
function NC_CreateFcn(hObject, eventdata, handles) %#ok<DEFNU>

    if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
        set(hObject,'BackgroundColor','white');
    end

function Helium_Callback(hObject, eventdata, handles) %#ok<DEFNU>

% --- Executes during object creation, after setting all properties.
function Helium_CreateFcn(hObject, eventdata, handles) %#ok<DEFNU>

    if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
        set(hObject,'BackgroundColor','white');
    end

function Pss_Callback(hObject, eventdata, handles) %#ok<DEFNU>

% --- Executes during object creation, after setting all properties.
function Pss_CreateFcn(hObject, eventdata, handles) %#ok<DEFNU>

    if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
        set(hObject,'BackgroundColor','white');
    end

function coolantTemp_Callback(hObject, eventdata, handles) %#ok<DEFNU>

% --- Executes during object creation, after setting all properties.
function coolantTemp_CreateFcn(hObject, eventdata, handles) %#ok<DEFNU>

    if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
        set(hObject,'BackgroundColor','white');
    end

function Mflowss_Callback(hObject, eventdata, handles) %#ok<DEFNU>

% --- Executes during object creation, after setting all properties.
function Mflowss_CreateFcn(hObject, eventdata, handles) %#ok<DEFNU>

    if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
        set(hObject,'BackgroundColor','white');
    end

function Power_Callback(hObject, eventdata, handles) %#ok<DEFNU>

% --- Executes during object creation, after setting all properties.
function Power_CreateFcn(hObject, eventdata, handles) %#ok<DEFNU>

    if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
        set(hObject,'BackgroundColor','white');
    end

function file_path_Callback(hObject, eventdata, handles) %#ok<DEFNU>

% --- Executes during object creation, after setting all properties.
function file_path_CreateFcn(hObject, eventdata, handles) %#ok<DEFNU>

    if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
        set(hObject,'BackgroundColor','white');
    end

function startBatch_Callback(hObject, eventdata, handles) %#ok<DEFNU>

% --- Executes during object creation, after setting all properties.
function startBatch_CreateFcn(hObject, eventdata, handles) %#ok<DEFNU>

    if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
        set(hObject,'BackgroundColor','white');
    end

function batchSize_Callback(hObject, eventdata, handles) %#ok<DEFNU>

% --- Executes during object creation, after setting all properties.
function batchSize_CreateFcn(hObject, eventdata, handles) %#ok<DEFNU>

    if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
        set(hObject,'BackgroundColor','white');
    end

function execTime_Callback(hObject, eventdata, handles) %#ok<DEFNU>

% --- Executes during object creation, after setting all properties.
function execTime_CreateFcn(hObject, eventdata, handles) %#ok<DEFNU>

    if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
        set(hObject,'BackgroundColor','white');
    end

function startFile_Callback(hObject, eventdata, handles) %#ok<DEFNU>

% --- Executes during object creation, after setting all properties.
function startFile_CreateFcn(hObject, eventdata, handles) %#ok<DEFNU>

    if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
        set(hObject,'BackgroundColor','white');
    end



function mindtBox_Callback(hObject, eventdata, handles)
% hObject    handle to mindtBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of mindtBox as text
%        str2double(get(hObject,'String')) returns contents of mindtBox as a double


% --- Executes during object creation, after setting all properties.
function mindtBox_CreateFcn(hObject, eventdata, handles)
% hObject    handle to mindtBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function initial_maxdtBox_Callback(hObject, eventdata, handles)
% hObject    handle to initial_maxdtBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of initial_maxdtBox as text
%        str2double(get(hObject,'String')) returns contents of initial_maxdtBox as a double


% --- Executes during object creation, after setting all properties.
function initial_maxdtBox_CreateFcn(hObject, eventdata, handles)
% hObject    handle to initial_maxdtBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function final_maxdtBox_Callback(hObject, eventdata, handles)
% hObject    handle to final_maxdtBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of final_maxdtBox as text
%        str2double(get(hObject,'String')) returns contents of final_maxdtBox as a double


% --- Executes during object creation, after setting all properties.
function final_maxdtBox_CreateFcn(hObject, eventdata, handles)
% hObject    handle to final_maxdtBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function initial_endtimeBox_Callback(hObject, eventdata, handles)
% hObject    handle to initial_endtimeBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of initial_endtimeBox as text
%        str2double(get(hObject,'String')) returns contents of initial_endtimeBox as a double


% --- Executes during object creation, after setting all properties.
function initial_endtimeBox_CreateFcn(hObject, eventdata, handles)
% hObject    handle to initial_endtimeBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function endtimeBox_Callback(hObject, eventdata, handles)
% hObject    handle to endtimeBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of endtimeBox as text
%        str2double(get(hObject,'String')) returns contents of endtimeBox as a double


% --- Executes during object creation, after setting all properties.
function endtimeBox_CreateFcn(hObject, eventdata, handles)
% hObject    handle to endtimeBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function minorBox_Callback(hObject, eventdata, handles)
% hObject    handle to minorBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of minorBox as text
%        str2double(get(hObject,'String')) returns contents of minorBox as a double


% --- Executes during object creation, after setting all properties.
function minorBox_CreateFcn(hObject, eventdata, handles)
% hObject    handle to minorBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function majorBox_Callback(hObject, eventdata, handles)
% hObject    handle to majorBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of majorBox as text
%        str2double(get(hObject,'String')) returns contents of majorBox as a double


% --- Executes during object creation, after setting all properties.
function majorBox_CreateFcn(hObject, eventdata, handles)

if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

function restartBox_Callback(hObject, eventdata, handles)

% --- Executes during object creation, after setting all properties.
function restartBox_CreateFcn(hObject, eventdata, handles)

if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --- Executes on selection change in initial_condBox.
function initial_condBox_Callback(hObject, eventdata, handles)

% --- Executes during object creation, after setting all properties.
function initial_condBox_CreateFcn(hObject, eventdata, handles)

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --- Executes on button press in changeDir.
function changeDir_Callback(hObject, eventdata, handles)
    default_dir=get(handles.file_path,'String');
    directoryname = uigetdir(default_dir,'Pick a new directory');
    set(handles.file_path,'String',directoryname);

function actionstartBox_Callback(hObject, eventdata, handles)
% hObject    handle to actionstartBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of actionstartBox as text
%        str2double(get(hObject,'String')) returns contents of actionstartBox as a double


% --- Executes during object creation, after setting all properties.
function actionstartBox_CreateFcn(hObject, eventdata, handles)
% hObject    handle to actionstartBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function timestepcntrlBox_Callback(hObject, eventdata, handles)
% hObject    handle to timestepcntrlBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of timestepcntrlBox as text
%        str2double(get(hObject,'String')) returns contents of timestepcntrlBox as a double


% --- Executes during object creation, after setting all properties.
function timestepcntrlBox_CreateFcn(hObject, eventdata, handles)
% hObject    handle to timestepcntrlBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in buttonSingletube.
function buttonSingletube_Callback(hObject, eventdata, handles)
% hObject    handle to buttonSingletube (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of buttonSingletube


% --- Executes on button press in buttonAnnulus.
function buttonAnnulus_Callback(hObject, eventdata, handles)
% hObject    handle to buttonAnnulus (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of buttonAnnulus


% --- Executes during object creation, after setting all properties.
function buttonSingletube_CreateFcn(hObject, eventdata, handles)
% hObject    handle to buttonSingletube (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called


% --- Executes during object creation, after setting all properties.
function buttonAnnulus_CreateFcn(hObject, eventdata, handles)
% hObject    handle to buttonAnnulus (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called



function rampBox_Callback(hObject, eventdata, handles)
% hObject    handle to rampBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of rampBox as text
%        str2double(get(hObject,'String')) returns contents of rampBox as a double


% --- Executes during object creation, after setting all properties.
function rampBox_CreateFcn(hObject, eventdata, handles)
% hObject    handle to rampBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes during object creation, after setting all properties.
function uibuttongroup5_CreateFcn(hObject, eventdata, handles)
% hObject    handle to uibuttongroup5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called


% --- Executes during object creation, after setting all properties.
function buttonFbno_CreateFcn(hObject, eventdata, handles)
% hObject    handle to buttonFbno (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called


% --- Executes during object creation, after setting all properties.
function buttonFb121to110_CreateFcn(hObject, eventdata, handles)
% hObject    handle to buttonFb121to110 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called


% --- Executes during object creation, after setting all properties.
function buttonFb111to110_CreateFcn(hObject, eventdata, handles)
% hObject    handle to buttonFb111to110 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called


% --- Executes on button press in disableNC_box.
function disableNC_box_Callback(hObject, eventdata, handles)
% hObject    handle to disableNC_box (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of disableNC_box
