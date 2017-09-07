function plotter_mat(default_dir,sequence,firstInSeq)

    starting_column=3;  %for some reason sometimes needs to be 2, sometimes 3

    if ~sequence
    userChoice=menu('Choose your processing option','Point to a directory and process all .mat within it and all subdirectories', 'Point to a file');   

    %% get paths to files
        if userChoice==1
            %creates a lists of files in a chosen folder, based on a desired
            %string in the name
            [directory, processed_files_list]=fileFinder('processed',1,default_dir,1);

        elseif userChoice==2
                cd(default_dir)
                [processed_files_list,directory,~] = uigetfile('*.mat','Choose .r file to process','MultiSelect','on');  
    %             processed_files_list=cellfun(@(x)regexp(x,'processed'),processed_files_list,'UniformOutput', false);
                if ~iscell(processed_files_list)
                    processed_files_list={processed_files_list};
                end
                directory={directory};
        end
    else
        [directory, processed_files_list]=fileFinder('processed',1,default_dir,firstInSeq);
    end
    
    %define the number of files to process
    number_of_processed_files=numel(processed_files_list);

    %define parameters to be plotted
%     parameters2process={'vapgen','sattemp','tempf','tempg','p','quala','quals','qualhy','rho','floreg','velg','velf','htvat','voidg','htrnr','tmassv','mflowj','vvol'};
    parameters2process={'tempf','tempg','p','vapgen','quala','quals','htvat','voidg','htrnr','tmassv','mflowj','vvol','velg','velf','rho','floreg'};
%     parameters2process={'tempf','htrnr','vapgen','vvol'};
    parameters2process_secondary={'tempf_secondary','p_secondary','vapgen_secondary','htvat_secondary','velf_secondary','floreg_secondary','rho_secondary'};
    parametersAmount=numel(parameters2process);  
    parametersAmount_secondary=numel(parameters2process_secondary);


    %n_file counts only correct mat files        
    n_file=0; 
    
    %open figure used for plotting
    fx=figure('visible','off');
    ax=axes; % create axes object to which code will plot
    colormap(fx,jet)
    
    %% Based of parameters2process variable, extract desired parameters from results files
    %start waitbar
%     h = waitbar(0,'Loading data, please wait');
    
    for n=1:number_of_processed_files
                
        clear num txt raw loc position data horz_tub_pos tempf_primarypipe fileName TPs PPs tmass tmass_mat Time Time_mat horz_tub_pos_mflowJ
        fileName=processed_files_list{n};
        
        %update wait bar and update text
%         waitbarString=['Loading file: ',fileName];
%         waitbarString=strrep(waitbarString,'_',' ');
%         waitbar(n/number_of_processed_files,h,waitbarString)
        
        % setup a container for another processed file to be easily read
        % with matlab GUI for the purpose of comparing against experiments
        exp_cmp_data.file=fileName;
%         fileName=fileName(1:end-4); %removes empty sign at the beginning (\n)
        disp(['Reading data from file: ',fileName])

        % there's another .mat file in the directory which is not with results
        % data, so this if clause ignores it
        if ~strcmp(fileName,'nodalization')

            n_file=n_file+1;
            directory_mat=cell2mat(directory(n));

            %define paths to files and to plots
            path_readFile=[directory_mat,'\',cell2mat(processed_files_list(n))]; %define path for reading current file
            pathPlots{n_file}=[directory_mat,'\Plots'];
            pathPlots_secondary{n_file}=[directory_mat,'\Plots\Secondary'];
            pathPlots_horz{n_file}=[directory_mat,'\Plots\Horizontal'];
            pathPlots_init{n_file}=[directory_mat,'\Plots\Initial_cond'];

            %read nodalization!
            temp_nod=load([directory_mat,'\nodalization.mat']);
            nodalization=temp_nod.nodalization;
            horz_tube_amount(n_file)=nodalization{5,2};
            heater_tank_height(n_file)=nodalization{1,2}+nodalization{2,2};
            condenser_start(n_file)= heater_tank_height(n_file)+nodalization{6,2};
            pipe_unit_length(n_file)=nodalization{7,2}*1000;

            %create directories for plots if the don't exist
            if exist(pathPlots{n_file},'dir')~=7
                mkdir(pathPlots{n_file});  
            end
            if exist(pathPlots_secondary{n_file},'dir')~=7
                mkdir(pathPlots_secondary{n_file});  
            end
            if exist(pathPlots_horz{n_file},'dir')~=7
                mkdir(pathPlots_horz{n_file});  
            end        
            if exist(pathPlots_init{n_file},'dir')~=7
                mkdir(pathPlots_init{n_file});  
            end

            temp_data=load(path_readFile);
            
            data=temp_data.varFull;
            %remove empty spaces
            data(:,1)=deblank(data(:,1));
            
                        % remove volume 113 % XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXYYYYYYYYYYYYYYYYZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ
                        offendingvolumelist=data(:,2);
                        data(find([offendingvolumelist{:}]==113010000),:)=[];
                        offendingvolumelist=data(:,2);
                        data(find([offendingvolumelist{:}]==114000000),:)=[];
                        % XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXYYYYYYYYYYYYYYYYZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ
            
            %get name of the file without suffixes 
            file_name=fileName(1:strfind(fileName,'_output_R')-1);
            %print primary pipe graph
            %different if parameters is for heat structure
            %BASED on nodalization.mat, figure out which values are for
            %primary side or secondary side values

            tube_vol=nodalization{1,2}+nodalization{2,2}+nodalization{3,2};   

            %if there are horizontal tube divisions, find their posisition
            if nodalization{5,2}>1
                for horz_tube_ctr=1:nodalization{5,2}
                    horz_tub_pos(horz_tube_ctr)=nodalization{1,2}+nodalization{2,2}+1+(horz_tube_ctr-1)*(nodalization{3,2});
                    horz_tub_pos_mflowJ(horz_tube_ctr)=nodalization{1,2}+nodalization{2,2}+2+(horz_tube_ctr-1)*((nodalization{3,2}-1));
                end
                horz_tub_pos_mflowJ(end+1)=horz_tub_pos_mflowJ(end)+nodalization{3,2}-1;
            end

            ss_start_vol=nodalization{1,2}+nodalization{2,2}+nodalization{3,2}*nodalization{5,2}+2;
            ss_end_vol=ss_start_vol+nodalization{4,2}-1;

            if nodalization{5,2}<3
                tube_vol_heatstr=nodalization{1,2}+nodalization{2,2}+nodalization{3,2};
                ss_start_heatstr=tube_vol_heatstr+1;  
            else
                tube_vol_heatstr=nodalization{1,2}+nodalization{2,2}+nodalization{3,2};
                ss_start_heatstr=nodalization{1,2}+nodalization{2,2}+nodalization{3,2}*2+2;
            end

            ss_end_heatstr=ss_start_heatstr+nodalization{4,2}-1;
            
            %initialize cleaning list used to remove variables not present in
            %results file, but requested
            removal_list=[];
            %find all values of all chosen parameters
            for o=1:parametersAmount
                %get data for currently processed parameter                
                parameter=parameters2process{o};
                parameter_secondary=[parameter,'_secondary'];
                parameter_horztube=[parameter,'_horztube'];

                loc=strcmp(parameter,data(:,1));
                position=find(loc);
                %verify that said parameters data is present
                if ~isempty(position)
                    paramValue_all=data(position(1):position(end),:);
                    horz_flag=1;
                    secondary_flag=1;
                    % depending on which parameter, and nodalization, find values for given parameters     
                    %these don't have HORIZONTAL components
                    if strcmp(parameter,'htvat')
                        horz_flag=0;
                        paramValue_primarypipe=cell2mat(paramValue_all(1:tube_vol_heatstr,starting_column:end));
                        paramValue_secondarypipe=cell2mat(paramValue_all((ss_start_heatstr-1):(ss_end_heatstr-1),starting_column:end));
                    elseif strcmp(parameter,'htrnr')
                        horz_flag=0;
                        second_htrnr_start=2*(nodalization{1,2}+nodalization{2,2}+nodalization{6,2})+2;  %+2 so start on the other side of HS
                        paramValue_primarypipe=cell2mat(paramValue_all(1:2:(2*tube_vol),starting_column:end));
                        paramValue_secondarypipe=cell2mat(paramValue_all((second_htrnr_start):2:(second_htrnr_start+2*nodalization{4,2}-2),starting_column:end));
                    elseif ismember(parameter,{'tmassv','qualhy','vvol','vollev'})  % THESE ARE NOT IN SECONDARY SIDE !!!!!!!
                        secondary_flag=0;
                        paramValue_primarypipe=cell2mat(paramValue_all(1:tube_vol,starting_column:end));  
                        if nodalization{5,2}>1
                            command_inter='paramValue_horztube_interleaved=reshape([';
                            for horztube_counter=1:numel(horz_tub_pos)
                                paramValue_horztube{horztube_counter}=cell2mat(paramValue_all(horz_tub_pos(horztube_counter):(horz_tub_pos(horztube_counter)+nodalization{3,2}-1),starting_column:end));
                                command_inter=[command_inter,'paramValue_horztube{',num2str(horztube_counter),'};'];
                            end

                            command_inter=[command_inter,'],',num2str(nodalization{3,2}),',[]);'];   % 30 - two times the number of vertical volumes
                            eval(command_inter);
                        end
                        elseif ismember(parameter,{'mflowj'})  % SPECIAL CASE, to include horizontal junction
                        secondary_flag=0;
                        paramValue_primarypipe=cell2mat(paramValue_all(1:tube_vol,starting_column:end));  
                        if nodalization{5,2}>1
                            command_inter='paramValue_horztube_interleaved=reshape([';
                            for horztube_counter=1:numel(horz_tub_pos_mflowJ)-1  % -1, because last value in this vector points to horizontal junction
                                paramValue_horztube{horztube_counter}=cell2mat(paramValue_all(horz_tub_pos_mflowJ(horztube_counter):(horz_tub_pos_mflowJ(horztube_counter)+nodalization{3,2}-1),starting_column:end));
                                command_inter=[command_inter,'paramValue_horztube{',num2str(horztube_counter),'};']; 
                            end
                            mflowjHorzjunTEMP=cell2mat(paramValue_all(horz_tub_pos_mflowJ(end):horz_tub_pos_mflowJ(end)+nodalization{3,2}-1,starting_column:end));
                            command_inter=[command_inter,'],',num2str(nodalization{3,2}),',[]);'];
                            eval(command_inter);
                        end
                    else
                        paramValue_primarypipe=cell2mat(paramValue_all(1:tube_vol,starting_column:end));
                        paramValue_secondarypipe=cell2mat(paramValue_all(ss_start_vol:ss_end_vol,starting_column:end));                
                        if nodalization{5,2}>1
                            command_inter='paramValue_horztube_interleaved=reshape([';
                            for horztube_counter=1:numel(horz_tub_pos)
                                    paramValue_horztube{horztube_counter}=cell2mat(paramValue_all(horz_tub_pos(horztube_counter):(horz_tub_pos(horztube_counter)+nodalization{3,2}-1),starting_column:end));
                                    command_inter=[command_inter,'paramValue_horztube{',num2str(horztube_counter),'};'];
                            end

                            command_inter=[command_inter,'],',num2str(nodalization{3,2}),',[]);'];   % 30 - two times the number of vertical volumes
                            eval(command_inter);
                        end
                    end

                    %from to are set to last, to plot only last plot
                    from=numel(paramValue_primarypipe(1,:));
                    to=numel(paramValue_primarypipe(1,:));

                    %prepare list of files for plotting legend

                    file_list_plot{n_file}=file_name;
                    file_list_plot_clear=strrep(file_list_plot, '_',' ');

                    %plot & save to workspace

                    command1=[parameter,'{n_file,1}=file_name;']; 
                    command2=[parameter,'{n_file,2}=paramValue_primarypipe;']; %/sat_temp_inlet;');%'-sat_temp_inlet;');
                    exp_cmp_data.primary.(parameter)=paramValue_primarypipe;
                    eval(command1);
                    eval(command2);
                    
                    
                    
                    % save secondary side
                    if secondary_flag
                        command3=[parameter_secondary,'{n_file,1}=file_name;']; 
                        command4=[parameter_secondary,'{n_file,2}=paramValue_secondarypipe;']; %/sat_temp_inlet;');%'-sat_temp_inlet;');
                        exp_cmp_data.secondary.(parameter)=paramValue_secondarypipe;
                        eval(command3);
                        eval(command4);    
                    end
                    
                    % save values for tube + annulus
                    if nodalization{5,2}>1 && horz_flag
                        command5=[parameter_horztube,'{n_file,1}=file_name;']; 
                        command6=[parameter_horztube,'{n_file,2}=paramValue_horztube_interleaved;']; %/sat_temp_inlet;');%'-sat_temp_inlet;');
                        eval(command5);
                        eval(command6); 
                        % if there's a horizontal junction, also include it in
                        % the results
                        if strcmp(parameter,'mflowj')
                            mflowjHorzjun{n_file,1}=file_name;
                            mflowjHorzjun{n_file,2}=mflowjHorzjunTEMP;
                            parameters2process{end+1}='mflowjHorzjun';
                        end
                    end
                else
                    %if no values were found for the parameter, delete it
                    %from the processing list
                    removal_list=[removal_list,o];
                end
            end
            
            %clear variabls not found in the results file
            parameters2process(removal_list)=[];
            parametersAmount=numel(parameters2process);
            %% Define x-axis for plots - time 

            %if clause below gets x data from time stored in output file
            time_row_no=1416;
            if strcmp(data(time_row_no,1),'time')
                Time=data(time_row_no,starting_column:end);
            else
                time_pos = find(cellfun(@(x) any(strcmp(x,'time')),data));
                Time=data(time_pos,starting_column:end);
                disp('position of time cell has changed - for performance adjust line 251 in plotter.mat');
                disp(['for time_row_no use the following position: ',num2str(time_pos)]);              
            end

            for celmat_time=1:numel(Time)
                Time_mat(celmat_time)=cell2mat(Time(celmat_time));
            end
            Time_mat_cell{n_file}=Time_mat;

        %% Check and plot mass balance
            tmass_row_no=1417;
            if strcmp(data(tmass_row_no,1),'tmass')
                tmass=data(tmass_row_no,starting_column:end);
            else
                tmass_pos = find(cellfun(@(x) any(strcmp(x,'tmass')),data));
                tmass=data(tmass_pos,starting_column:end);
                disp('position of tmass cell has changed - for performance adjust line 267 in plotter.mat');
                disp(['for tmass_row_no use the following position: ',num2str(tmass_pos)]);       
            end


            for tmass_time=1:numel(tmass)
                tmass_mat(tmass_time)=cell2mat(tmass(tmass_time));
            end
            tmass_mat_cell{n_file}=tmass_mat;


            %plot for each file
            disp('Plotting mass balance')
            
            current_file_name=file_list_plot(n_file);
            current_file_name_char=[current_file_name{1}(1:end)];
            path_print=[pathPlots{n_file},'\tmass_',current_file_name_char];
%             set(0,'CurrentFigure',fx) % make sure current figure didn't change (happens when user clicks on GUI during execution of this)
            plot(ax,Time_mat_cell{n_file},tmass_mat_cell{n_file});
            xlabel(ax,'Time [s]')
            ylabel(ax,'Tmass [kg]')


            %-------------------------------------------------------------------------------------------------
            %        PLOTTING COMMAND:  surf(Time,pipeLength,p{plot_counter,2})
            %                           imagesc(Time,pipeLength,tempg{plot_counter,2}); colorbar;
            %                           set(gca,'YDir','normal')
            %-------------------------------------------------------------------------------------------------
            
            % print to file
            saveas(fx,path_print,'png')

            cla(ax,'reset')
            
            %% Check and plot mass error
            emass_row_no=52;
            if strcmp(data(emass_row_no,1),'emass')
                emass=data(emass_row_no,starting_column:end);
            else
                emass_pos = find(cellfun(@(x) any(strcmp(x,'emass')),data));
                emass=data(emass_pos,starting_column:end);
                disp('position of emass cell has changed - for performance adjust line 326 in plotter.mat');
                disp(['for tmass_row_no use the following position: ',num2str(emass_pos)]);       
            end


            for emass_time=1:numel(emass)
                emass_mat(emass_time)=cell2mat(emass(emass_time));
            end
            emass_mat_cell{n_file}=emass_mat;


            %plot for each file
            disp('Plotting mass error')
%             colormap(fx,jet)
%             current_file_name=file_list_plot(n_file);
%             current_file_name_char=[current_file_name{1}(1:end)];
            path_print=[pathPlots{n_file},'\emass_',current_file_name_char]; % current file name char taken from above for tmass
%             set(0,'CurrentFigure',fx) % make sure current figure didn't change (happens when user clicks on GUI during execution of this)
            plot(ax,Time_mat_cell{n_file},emass_mat_cell{n_file}(1:numel(Time_mat_cell{n_file})));
            xlabel(ax,'Time [s]')
            ylabel(ax,'Mass err [kg]')


            %-------------------------------------------------------------------------------------------------
            %        PLOTTING COMMAND:  surf(Time,pipeLength,p{plot_counter,2})
            %                           imagesc(Time,pipeLength,tempg{plot_counter,2}); colorbar;
            %                           set(gca,'YDir','normal')
            %-------------------------------------------------------------------------------------------------
            
            % print to file
            saveas(fx,path_print,'png')

            cla(ax,'reset')
            

        end
        save([directory{n},'\',fileName(1:strfind(fileName,'_output_R')-1),'_simplified_for_Matlab'],'exp_cmp_data')
    end
    %close waitbar 
%     close(h) 

    %% Primary Side Plot
    %plot results on graphs
    clear data %free up some meory
    disp('')
    disp('*********************************************')
    disp('Plotting primary side parameters')
    
    
    %start waitbar
%     h = waitbar(0,'Plotting primary side data');
%     XXXXXXXXXXXXXXXXXXXXXXX
    for parameter_counter=1:parametersAmount 
    
        printed_parameter=parameters2process{parameter_counter};
        disp(['Plotting ',printed_parameter])
    
        %plotting loop, goes through all files
        for plot_counter=1:n_file
            clear p_avg                       

            colormap(fx,jet)
            current_file_name=file_list_plot(plot_counter);
            current_file_name_char=current_file_name{1}(1:end);
            path_print=[pathPlots{plot_counter},'\',printed_parameter,'_',current_file_name_char];
            command_size=['numel(',printed_parameter,'{plot_counter,2})'];
            parameter_size=eval(command_size);
            
%             %update wait bar and update text
%             waitbarString=['Plotting parameter: ', printed_parameter,' for file: ',current_file_name_char];    XXXXXXXXXXXXXXXXXXXXXXX
%             waitbarString=strrep(waitbarString,'_',' '); XXXXXXXXXXXXXXXXXXXXXX
%             waitbar(((parameter_counter-1)*n_file+n_file)/(parametersAmount*n_file),h,waitbarString) XXXXXXXXXXXXXXXXXXXXX
%             
%             
% %             fx.Visible='off'; %makes axes invisible
%             figure(fx,'Visible','Off') XXXXXXXXXXXXXXXXXXXXX
            
            pipeLength=(10:pipe_unit_length(plot_counter):((parameter_size-1)*pipe_unit_length(plot_counter))+10);  %#ok<NASGU>
%             command_plot=['imagesc(Time_mat_cell{plot_counter},pipeLength,',printed_parameter,'{plot_counter,2}); colorbar;'];
            command_plot=['imagesc(',printed_parameter,'{plot_counter,2}); colorbar;'];
            eval(command_plot);
            xlabel('Time [s]')
            ylabel(printed_parameter)
            set(ax,'YDir','normal')
            %fix time axis
            set(ax,'XTickMode','manual')
            Xtick_fixed=get(ax,'Xtick')*(Time_mat(end-1)-Time_mat(end-2));
            set(ax,'XTickLabels',Xtick_fixed);
                       
            hold(ax,'on')
            x1_prim=-0.5;
            x2_prim=Time_mat_cell{plot_counter}(end);
            y1_prim=heater_tank_height(plot_counter)*pipe_unit_length(plot_counter);
%             y2_prim=y1_prim;

            y1_prim_2=condenser_start(plot_counter)*pipe_unit_length(plot_counter);
%             y2_prim_2=y1_prim_2;

            line(ax,[x1_prim,x2_prim],[y1_prim,y1_prim],'Color',[1 1 1])    
            line(ax,[x1_prim,x2_prim],[y1_prim_2,y1_prim_2],'Color',[1 1 1])    
    %-------------------------------------------------------------------------------------------------
    %        PLOTTING COMMAND:  surf(Time,pipeLength,p{plot_counter,2})
    %                           imagesc(Time,pipeLength,tempg{plot_counter,2}); colorbar;
    %                           set(gca,'YDir','normal')
    %-------------------------------------------------------------------------------------------------
            % print to file
            saveas(fx,path_print,'png')

            cla(ax,'reset')


            % additional mass balance only for primary side
            if strcmp(printed_parameter,'tmassv')
                tmassv_sum=sum(tmassv{plot_counter,2});
          
%                 set(0,'CurrentFigure',fx) % make sure current figure didn't change (happens when user clicks on GUI during execution of this)
                plot(ax,Time_mat_cell{plot_counter},tmassv_sum)
                xlabel(ax,'Time [s]')
                ylabel(ax,'Primary side total mass [kg]')
                set(ax,'YDir','normal')
                path_print=[pathPlots{plot_counter},'\tmassv_sum_',current_file_name_char];
                saveas(fx,path_print,'png')

                cla(ax,'reset')

                % average p - maybe write this so average X is possible
            elseif strcmp(printed_parameter,'p')
                p_amount=size(p{plot_counter,2});
                for p_counter=1:p_amount(2)
                    p_avg(p_counter)=sum(p{plot_counter,2}(:,p_counter))/p_amount(1);
                end

%                 set(0,'CurrentFigure',fx) % make sure current figure didn't change (happens when user clicks on GUI during execution of this)
                plot(ax,Time_mat_cell{plot_counter},p_avg)
                xlabel(ax,'Time [s]')
                ylabel(ax,'Primary side avg press [Pa]')
                set(ax,'YDir','normal')
                path_print=[pathPlots{plot_counter},'\press_avg_',current_file_name_char];
                saveas(fx,path_print,'png')

                cla(ax,'reset')
                 %additionally plot vapgen in heater tank (so evaporated steam)
            elseif strcmp(printed_parameter,'vapgen')
%                 evap_steam_rate=vapgen{plot_counter,2}(1:nodalization{1,2}+nodalization{2,2},:);
%                 evap_steam_rate=vapgen{plot_counter,2}(1,:);   %only in lowest volume kg/(s*m3)
                steam_evap_rate_boiler=vapgen{plot_counter,2}(1:nodalization{1,2},:);   %in the boiler lowest volume kg/(s*m3)
                                
                %multiply by volume to get steam mass flow
                % 4.4018e-4 is volume of evaporation place in m3
                try
                    heater_vol=vvol{plot_counter,2}(1,1);
                catch
                    heater_vol=4.4018e-4;
                end
                steam_evap_flow_boiler{plot_counter}=sum(steam_evap_rate_boiler.*heater_vol);
                steam_evap_heat=steam_evap_flow_boiler{plot_counter}*2100000; %2100000 J/kg  - condensation Power - rougly only
                
%                 set(0,'CurrentFigure',fx) % make sure current figure didn't change (happens when user clicks on GUI during execution of this)
                plot(ax,Time_mat_cell{plot_counter},steam_evap_heat,'.')
                xlabel(ax,'Time [s]')
                ylabel(ax,'Evaporation power [W]')
                set(ax,'YDir','normal')
                path_print=[pathPlots{plot_counter},'\evap_power_',current_file_name_char];
                saveas(fx,path_print,'png')

                cla(ax,'reset')
                
                %store condensation in empty volume in heater tank
                steam_cond_rate_heater_empty=vapgen{plot_counter,2}(nodalization{1,2}+1:nodalization{1,2}+nodalization{2,2},:);   %in the boiler lowest volume kg/(s*m3)                
                %multiply by volume to get steam mass flow
                % 4.4018e-4 is volume of evaporation place in m3
                steam_cond_flow_heater_empty{plot_counter}=sum(steam_cond_rate_heater_empty.*heater_vol);

            elseif strcmp(printed_parameter,'mflowj')
                mflowj111=mflowj{plot_counter,2}(6,:);
                mflowj115=mflowj{plot_counter,2}(8,:);
                mflowj116=mflowj{plot_counter,2}(9,:);
                if nodalization{5,2}>1
                    mflowj130=sum(mflowjHorzjun{plot_counter});
                end
                
%                 set(0,'CurrentFigure',fx) % make sure current figure didn't change (happens when user clicks on GUI during execution of this)
                hold(ax,'on')
                plot(ax,Time_mat_cell{plot_counter},mflowj111)
                plot(ax,Time_mat_cell{plot_counter},mflowj115)
                plot(ax,Time_mat_cell{plot_counter},mflowj116)
                
                if nodalization{5,2}>1
                    mflowj130=sum(mflowjHorzjun{plot_counter,2});
                    plot(ax,Time_mat_cell{plot_counter},mflowj130)
                    legend(ax,'mflowj 111','mflowj 115','mflowj 116','mflowj 130 (horz)')
                else
                    legend(ax,'mflowj 111','mflowj 115','mflowj 116')
                end
                
                xlabel(ax,'Time [s]')
                ylabel(ax,'Mflow [kg/s]')
                set(ax,'YDir','normal')
                path_print=[pathPlots{plot_counter},'\mflowj_time_',current_file_name_char];
                saveas(fx,path_print,'png')

                cla(ax,'reset')

                
            end
            %plot initial conditions

            current_file_name=file_list_plot(plot_counter);
            current_file_name_char=current_file_name{1}(1:end);
            path_print_init=[pathPlots_init{plot_counter},'\',printed_parameter,'_',current_file_name_char];
            command_size=['size(',printed_parameter,'{plot_counter,2})'];
            parameter_size=eval(command_size);

            pipeLength=(10:pipe_unit_length(plot_counter):((parameter_size(1)-1)*pipe_unit_length(plot_counter))+10); %#ok<NASGU>
            command_plot=['plot(',printed_parameter,'{plot_counter,2}(:,1),pipeLength);'];
            eval(command_plot);
            ylabel('Tube length [mm]')
            xlabel(printed_parameter)
            set(gca,'YDir','normal')
            saveas(fx,path_print_init,'png')

            cla(ax,'reset')
        end

    end
    %close waitbar
%     close(h)  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

    %% Secondary Side Plot
    %plot results on graphs
    disp('')
    disp('*********************************************')
    disp('Plotting secondary side parameters')
%     axes(fx)
%     fx.Visible='off'; %make axes invisible
    for parameter_counter_secondary=1:parametersAmount_secondary
    
        printed_parameter_secondary=parameters2process_secondary{parameter_counter_secondary};
        disp(['Plotting ',printed_parameter_secondary])
        %plotting loop, goes through all files
        for plot_counter=1:n_file
            
            colormap(fx,jet)
            current_file_name=file_list_plot(plot_counter);
            current_file_name_char=current_file_name{1}(1:end);
            path_print_secondary=[pathPlots_secondary{plot_counter},'\',printed_parameter_secondary,'_',current_file_name_char];
            command_size=['size(',printed_parameter_secondary,'{plot_counter,2})'];
            parameter_size=eval(command_size);

            pipeLength=(10:pipe_unit_length(plot_counter):((parameter_size(1)-1)*pipe_unit_length(plot_counter))+10); %#ok<NASGU>
%             command_plot=['imagesc(Time_mat_cell{plot_counter},pipeLength,',printed_parameter_secondary,'{plot_counter,2}); colorbar;'];
            command_plot=['imagesc(',printed_parameter_secondary,'{plot_counter,2}); colorbar;'];
            eval(command_plot);
            xlabel('Time [s]')
            ylabel(printed_parameter_secondary)
            set(gca,'YDir','normal')

    %-------------------------------------------------------------------------------------------------
    %        PLOTTING COMMAND:  surf(Time,pipeLength,p{plot_counter,2})
    %                           imagesc(Time,pipeLength,tempg{plot_counter,2}); colorbar;
    %                           set(gca,'YDir','normal')
    %-------------------------------------------------------------------------------------------------
            % print to file
            saveas(fx,path_print_secondary,'png')

            cla(ax,'reset')

        end

    end

    %% Horizontal plot
    disp('')
    disp('*********************************************')
    disp('Plotting primary side parameters - horizontal')
    for parameter_counter=1:parametersAmount
    
        printed_parameter=parameters2process{parameter_counter};
        disp(['Plotting ',printed_parameter])
        
        %plotting loop, goes through all files
        plot_counter=1;
        for a=1:n_file
                clear quala_avg 
                clear vapgen_sum
            if horz_tube_amount(plot_counter)>1            
                try
    %                 colormap(fx,jet)
                    current_file_name=file_list_plot(plot_counter);
                    current_file_name_char=current_file_name{1}(1:end);
                    path_print_horz=[pathPlots_horz{plot_counter},'\',printed_parameter,'_',current_file_name_char];
                    command_size=['size(',printed_parameter,'_horztube{plot_counter,2})'];
                    parameter_size=eval(command_size);

                    %plot last n iteration steps
                    last_n=5;  %XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                
                    current_var_command=[printed_parameter,'_horztube{plot_counter,2}'];
                    current_var=eval(current_var_command);
                    %verify there's enough data for last_n iteration to be
                    %displayed
                    [~,dataArrWidth]=size(current_var);
                    if last_n>dataArrWidth/2
                        last_n=dataArrWidth/2;
                    end
                    last_n_iter=current_var(:,(end-last_n*horz_tube_amount(plot_counter)+1):end);
                    pipeLength=(10:pipe_unit_length(plot_counter):((parameter_size(1)-1)*pipe_unit_length(plot_counter))+10); %#ok<NASGU>
                    imagesc(last_n_iter);
                    colorbar;

                    %add vertical lines between the separate snapshots
                    hold(ax,'on')
                    y1=0;
                    y2=parameter_size(1)+0.5;

                    for line_ctr=1:last_n %(parameter_size(2)/horz_tube_amount(plot_counter))
                        x1=horz_tube_amount(plot_counter)*line_ctr+0.5;
                        line(ax,[x1,x1],[y1,y2],'Color',[1 1 1])
                    end

                    %add labeling
                    xlabel(ax,'Time [s]')
                    ylabel(ax,printed_parameter)
                    set(ax,'YDir','normal')

            %-------------------------------------------------------------------------------------------------
            %        PLOTTING COMMAND:  surf(Time,pipeLength,p{plot_counter,2})
            %                           imagesc(Time,pipeLength,tempg{plot_counter,2}); colorbar;
            %                           set(gca,'YDir','normal')
            %-------------------------------------------------------------------------------------------------
                    % print to file
                    saveas(fx,path_print_horz,'png')

                    cla(ax,'reset')


                catch ME
%                     rethrow(ME)
                end

                %print avg NC fraction vs time for every file
                if strcmp(printed_parameter,'quala')
                    quala_size=size(current_var);

                    % since for every time step has n columns
                    % n=horz_tube_amount(plot_counter)
                    % it has to be summed and averaged for those n columns for
                    % each time step
                    % number of time steps is horizontal length of data matrix
                    % divided by columns number
                    for quala_counter=1:quala_size(2)/horz_tube_amount(plot_counter)
                        quala_sum=0;
                        % the second for loop sums over all the columns
                        % belonging to a single time step (starting at last)
                        for horz_cnt=1:horz_tube_amount(plot_counter)
                            summing_cnt=quala_counter*horz_tube_amount(plot_counter)-(horz_tube_amount(plot_counter)-horz_cnt);
                            quala_sum=quala_sum+sum(current_var(:,summing_cnt));
    %                         if quala_counter==1;
    %                             summing_cnt
    %                         end
                        end
                        quala_avg(quala_counter)=quala_sum/(horz_tube_amount(plot_counter)*quala_size(1));
                    end
                    
%                     set(0,'CurrentFigure',fx) % make sure current figure didn't change (happens when user clicks on GUI during execution of this)
                    plot(ax,Time_mat_cell{plot_counter},quala_avg)
                    xlabel(ax,'Time [s]')
                    ylabel(ax,'Primary side avg NC quality')
                    set(ax,'YDir','normal')
                    path_print=[pathPlots{plot_counter},'\quala_avg_',current_file_name_char];
                    saveas(fx,path_print,'png')
                    cla(ax,'reset')

                end

                %calculate and print integral vapgen vs time for every file
                %(only in test tube)
                 %print avg NC fraction vs time for every file
                if strcmp(printed_parameter,'vapgen')
                    vapgen_size=size(current_var);
                    try
                        inntube_vol=vvol_horztube{plot_counter,2}(1,1);
                        annulus_vol=vvol_horztube{plot_counter,2}(1,2);
                    catch
                    inntube_vol=6.2832e-6;
                    annulus_vol=1.885e-5;
                    end
                    volume_array=[inntube_vol annulus_vol];   % volumes of inner tube sections and annulus section - necessary to calculate kg/s from kg/(m3*s)
                    
                    % since for every time step has n columns
                    % n=horz_tube_amount(plot_counter)
                    % it has to be summed and averaged for those n columns for
                    % each time step
                    % number of time steps is horizontal length of data matrix
                    % divided by columns number
                   for vapgen_sum_counter=1:vapgen_size(2)/horz_tube_amount(plot_counter)
                        vapgen_sum_tmp=0;
                        % the second for loop sums over all the columns
                        % belonging to a single time step (starting at last)
                        for horz_cnt=1:horz_tube_amount(plot_counter)
                            vapgen_summing_cnt=vapgen_sum_counter*horz_tube_amount(plot_counter)-(horz_tube_amount(plot_counter)-horz_cnt);
                            vapgen_sum_tmp=vapgen_sum_tmp+sum(current_var(:,vapgen_summing_cnt))*volume_array(horz_cnt);
                        end
                        % 3.77e-04 is tube volume in m3, and since vapgen is in
                        % kg/m3.s, then mupltiplying by volume leaves us with
                        % kg/s - comparable to experiment
                        vapgen_sum(vapgen_sum_counter)=vapgen_sum_tmp;
                   end

                    condflux{a}=vapgen_sum;

                end

            end
        plot_counter=plot_counter+1;
        end
    end
    
    %plot mass balance
    disp('Plotting fixed mass fluxes on one graph')
    
    for mb=1:numel(steam_evap_flow_boiler)  
       
        %figure out file for current iteration
        current_file_name=file_list_plot(mb);
        current_file_name_char=current_file_name{1}(1:end);
        
        %calculate final mean values for last nStep steps
        %while loop is used to decrease nStep in case the total number of
        %written steps is less than desired value
        meanFlag=1;
        nStep=10;
        while meanFlag && nStep>0
            try
                boilFinal=mean(steam_evap_flow_boiler{mb}(end-nStep:end));
                condEmptyFin=mean(steam_cond_flow_heater_empty{mb}(end-nStep:end));
                condTubeFinal=mean(condflux{mb}(end-nStep:end));
                meanFlag=0; % if execution reached this far, then the while loop may be finished
            catch
                nStep=nStep-1;
            end
        end
        sumFinal=boilFinal+condEmptyFin+condTubeFinal;
        
        %plot
%         set(0,'CurrentFigure',fx) % make sure current figure didn't change (happens when user clicks on GUI during execution of this)
        hold(ax,'on')
        plot(ax,Time_mat_cell{mb},steam_evap_flow_boiler{mb})
        plot(ax,Time_mat_cell{mb},steam_cond_flow_heater_empty{mb},'--')
        plot(ax,Time_mat_cell{mb},-condflux{mb},'.-')
        legend(ax,['Boiler evaporation flow: ',num2str(boilFinal),'kg/s'],...
            ['Empty heater cond flow: ',num2str(condEmptyFin),'kg/s'],...
            ['Tube condensation flow: ',num2str(condTubeFinal),'kg/s'],...
            'Location','southoutside')
        xlabel(ax,'Time [s]')
        ylabel(ax,'Condensation mass flux [kg/s]')
        title(ax,['Final integral massflow: ',num2str(sumFinal),' kg/s'])
%         set(gca,'YDir','normal')
        %save
        path_print=[pathPlots{mb},'\mass_fluxes_',current_file_name_char];
        saveas(fx,path_print,'png')

        cla(ax,'reset')

        
        %extra plot - total vapgen vs delta P
        
        deltaP=diff(mean(p{mb,2}));        
        vapgenIntegral=steam_evap_flow_boiler{mb}+steam_cond_flow_heater_empty{mb}+condflux{mb};
%         set(0,'CurrentFigure',fx) % make sure current figure didn't change (happens when user clicks on GUI during execution of this)
        plot(ax,deltaP)
        ylabel(ax,'Pressure change [bar]')
        yyaxis (ax,'right')
        plot(ax,vapgenIntegral)
        ylabel(ax,'Integral mass balance [ks/s]')
        xlabel(ax,'Time [s]')
        legend(ax,'Press diff','Integral mass balance')
%         set(ax,'YDir','normal')
        
        % align zero for left and right
        yyaxis(ax,'right'); ylimr = get(ax,'Ylim');ratio = ylimr(1)/ylimr(2);
        yyaxis(ax,'left'); yliml = get(ax,'Ylim');
        if ~(yliml(1)==0&&ylimr(1)==0)
            if yliml(2)*ratio<yliml(1)
                set(ax,'Ylim',[yliml(2)*ratio yliml(2)])
            else
                set(ax,'Ylim',[yliml(1) yliml(1)/ratio])
            end
        end

        %save
        path_print=[pathPlots{mb},'\integralVapgen_pDiff_',current_file_name_char];
        saveas(fx,path_print,'png')
        
        cla(ax,'reset')
        
    end
    
    %plot heat balance
    
    disp('Plotting fixed heat balance')
    
    for mb=1:numel(htrnr)/2 % /2 beacuse of name + data  
        current_file_name=file_list_plot(mb);
        current_file_name_char=current_file_name{1}(1:end);
        
        heater_side_area=pi*0.0837*nodalization{7,2};
        annulus_side_area=pi*0.02*nodalization{7,2};
        coolant_side_area=pi*0.03*nodalization{7,2};
        
        heat_heater=sum(htrnr{mb,2}(1:nodalization{1,2},:)*heater_side_area);
        heat_empty=sum(htrnr{mb,2}((nodalization{1,2}+1):(nodalization{1,2}+nodalization{2,2}),:)*heater_side_area);
        heat_condenser=-sum(htrnr{mb,2}((nodalization{1,2}+nodalization{2,2}):end,:)*annulus_side_area);
        heat_coolant=sum(htrnr_secondary{mb,2}*coolant_side_area);
        
        colormap(fx,jet) 
%         set(0,'CurrentFigure',fx) % make sure current figure didn't change (happens when user clicks on GUI during execution of this)
        hold(ax,'on')
        plot(ax,Time_mat_cell{mb},heat_heater)
        plot(ax,Time_mat_cell{mb},heat_empty)
        plot(ax,Time_mat_cell{mb},heat_condenser)
        plot(ax,Time_mat_cell{mb},heat_coolant)
        legend(ax,'Heat delivered','Empty heater','Tube condensation heat','Coolant heat pickup','Location','bestoutside')
        xlabel(ax,'Time [s]')
        ylabel(ax,'Heat [W]')
%         ylim([0,1.1*mean(heat_heater)]);
        set(ax,'YDir','normal')
        path_print=[pathPlots{mb},'\heat_balance_',current_file_name_char];
        saveas(fx,path_print,'png')

        cla(ax,'reset')

    end
    
    %close invisible figure
    close(fx)
    disp('*********************************************')
    disp('Plotting finished')

end