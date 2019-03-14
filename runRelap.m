function directory=runRelap(dirCode,default_dir,starting_file,execution_time,starting_batch,batch_size,sequence,firstInSeq)
    
    %set license as enviromental variable
    dirCode=[dirCode,'\'];
    try
        lic_file=fopen([dirCode,'license.txt']);
    catch
        disp('Relap license not found - verify path to RELAP directory')
    end
    license=textscan(lic_file,'%s');
    fclose(lic_file);
    setenv('rslicense',char(license{1}));
    %REMEMBER ABOUT SETTING UP ENV VAR FOR RS LICENSE!!!!!

    %import input decks list form file created by runRelapCalculation.m
    clear input_decks_list input_decks_number
    if ~sequence
        userChoice=menu('Choose your processing option','Point to a directory and process all .i files within it and all subdirectories', 'Point to a file');   

        if userChoice==1
            %creates a lists of files in a chosen folder, based on a desired
            %string in the name
            [directory,input_decks_list]=fileFinder('.i',1,default_dir,1);
        elseif userChoice==2

                [input_decks_list,directory,~] = uigetfile('*.i','Choose .i file to process','MultiSelect','on');   
                input_decks_list=input_decks_list(1:end-2);  %removes .i
                if ~iscell(input_decks_list)
                    input_decks_list={input_decks_list};
                end
                directory=directory(1:end-1); %removes \ sign
                if ~iscell(directory)
                    directory={directory};
                end
        end
    else
        [directory, input_decks_list]=fileFinder('.i',1,default_dir,firstInSeq);
    end

    input_decks_number=numel(input_decks_list);
    batch_amount=(input_decks_number-starting_file+1)/batch_size;

    %determine number of batches to process:
    if (batch_amount>round(batch_amount))
        batch_count=round(batch_amount)+1;
    elseif (batch_amount<round(batch_amount))
        batch_count=round(batch_amount);
    else
        batch_count=batch_amount;
    end

    %vary batch size if there's less files than batch size to process
    if batch_amount<1
        batch_size=input_decks_number;
    end
   
    %inform user about status
    disp(['Number of input decks to run: ',num2str(input_decks_number),' in ',num2str(batch_count),' batches'])
    disp(' ')
    disp(['Calculations started at:             ',datestr(datetime('now'))])
    disp(['Calculations will be completed at:   ',datestr(datetime('now')+seconds(batch_count*execution_time))])
    disp(' ')
    
    %processing loop (outer loop runs batches for an hour, inner loop opens
    %number of files, specified in batch_size at once in Relap5
    for m=starting_batch:batch_count
        disp(['Calculating batch ',num2str(m)])
        for n=starting_file:starting_file+batch_size-1

            fileName=input_decks_list{n};
            fileDir=directory{n};
            inputFile=[fileDir,'\',fileName,'.i -o '];
            outputOfile=[fileDir,'\',fileName(1:end-11),'_output_O.o'];
            outputRfile=[fileDir,'\',fileName(1:end-11),'_output_R.r'];
            %check if o and r files already exists - RELAP cannot continue
            %if they do - either stop calculation, or delete
            r_exist=exist(outputOfile,'file');
            o_exist=exist(outputRfile,'file');
            if r_exist && ~o_exist
                button = questdlg('RELAP .o already exists.','Action required','Overwrite','Keep','Stop code','Overwrite');
                if strcmp(button,'Overwrite')
                    delete(outputOfile)
                elseif strcmp(button,'Keep')
                    outputOfile=[outputOfile(1:end-2),'v2','.o'];
                else
                    error('Execution interrupted')
                end
            elseif ~r_exist && o_exist
                button = questdlg('RELAP .r file already exists.','Action required','Overwrite','Keep','Stop code','Overwrite');
                if strcmp(button,'Overwrite')
                    delete(outputRfile)
                elseif strcmp(button,'Keep')
                    outputRfile=[outputRfile(1:end-2),'v2','.r'];
                else
                    error('Execution interrupted')
                end
            elseif r_exist && o_exist
                button = questdlg('RELAP .r and o. files already exist.','Action required','Overwrite','Keep','Stop code','Overwrite');
                if strcmp(button,'Overwrite')
                    delete(outputOfile)
                    delete(outputRfile)
                elseif strcmp(button,'Keep')
                    outputOfile=[outputOfile(1:end-2),'v2','.o'];
                    outputRfile=[outputRfile(1:end-2),'v2','.r'];
                else
                    error('Execution interrupted')
                end
            end
            
            command=[dirCode,'relap5 -i ',inputFile,outputOfile,' -r ',outputRfile,' -w ',dirCode,'tpfh2o &'];
%             command=cell2mat(command); %convert cell array to a matrix
            dos(command);
          
        end

        processed_amount=m*batch_size;
        starting_file=processed_amount+1; %set the start file of the next batch (first after all processed ones)
        left_to_process=input_decks_number-processed_amount;
        %if there's less to process than batch_size, assign this value to
        %batch_size
        if (left_to_process<batch_size)
            batch_size=left_to_process;
        end

        %control the calculation time

        pause(execution_time/4)
        disp([datestr(datetime('now')),'        1/4 of execution time passed: ',num2str(execution_time*0.25),'s'])
        pause(execution_time/4)
        disp([datestr(datetime('now')),'        1/2 of execution time passed: ',num2str(execution_time*0.5),'s'])
        pause(execution_time/4)
        disp([datestr(datetime('now')),'        3/4 of execution time passed: ',num2str(execution_time*0.75),'s'])
        pause(execution_time/4)
        disp([datestr(datetime('now')),'        Execution finished after: ',num2str(execution_time),'s']) 
        disp(' ')
        
        dos('taskkill /im relap5.exe');
        disp(' ')
    end
    disp('All RELAP5 runs executed')
end