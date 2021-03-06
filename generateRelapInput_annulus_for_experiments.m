function directory=generateRelapInput_annulus_for_experiments(inputs,input_type,file_dir,NCmodelFlag)
    %% INPUT----------------------------------------------------
        
        if input_type
            % MANUAL INPUT
            %Primary side - initial conditions  ********************************************************************************

            Pps=str2double(get(inputs.Pps,'String'));         %Initial pressure [Bar]
            NC=str2double(get(inputs.NC,'String'));               %Non condensable mole fr (quality in relap)
            Helium=str2double(get(inputs.Helium,'String'));       %Mole fraction of Helium in NC mixture

            %Secondary side - initial and operating conditions ********************************************************************************
            Pss=str2double(get(inputs.Pss,'String'));
            Tss=str2double(get(inputs.coolantTemp,'String'));  %T_secondary_side = T_primary_side(Pressure_primary_side) - superheat
            Mflowss=str2double(get(inputs.Mflowss,'String'));  %secondary side mass flow [kg/h]

            %Heater ********************************************************************************
            Power=str2double(get(inputs.Power,'String')); % [W]
            %technicalities
            fileCounter=1;
            directory={file_dir};
        else
            % AUTO INPUT FROM PROCESSED EXPERIMENTAL DATA XLS FILES
            
            %generate list of input files             
            userChoice=menu('Choose your processing option','Point to a directory and process all .mat within it and all subdirectories', 'Point to a file');   

            if userChoice==1
                %creates a lists of files in a chosen folder, based on a desired
                %string in the name
                [directory,fileList]=fileFinder('RELAP_INPUT',1,file_dir,1);

            elseif userChoice==2
                cd(file_dir)
                [fileList,directory,~] = uigetfile('*.xls','Choose .r file to process','MultiSelect','on');  
                if ~iscell(fileList)
                    fileList={fileList};
                end
                directory={directory};
            end
            
            fileCounter=numel(fileList);
        
            
            %read xls input files
            for file_counter=1:numel(fileList)
                [num, ~]=xlsread([directory{file_counter},'\', fileList{file_counter}]);
                Pps(file_counter)=num(1);           %Primary side initial pressure [bar]
                NC(file_counter)=num(2);            %Primary side average NC content [1]
                Helium(file_counter)=num(3);        %Primary side He fraction in NC mixture [1]
                Pss(file_counter)=num(4);           %Secondary side pressure [bar]
                Tss(file_counter)=num(5);           %Secondary side temperature [C]
                Mflowss(file_counter)=num(6);       %Secondary side mflow [kg/h]
                Power(file_counter)=num(7);         %Delivered power [W]
            end
        end
        
        %% Additional parameters
        
        %Timing
        action_start=inputs.action_start; %[s] - when coolant water will start to flow and when heater will start to heat
        action_ramp=inputs.action_ramp; %[s] - how many seconds to go from 0 to full HF
        action_start_end=num2str(str2double(action_start)+str2double(action_ramp)); %when the heating will reach full HF
        
        %Relap Calculation parameters 
        mindt=inputs.mindt;
        initial_maxdt=inputs.initial_maxdt;
        final_maxdt=inputs.final_maxdt;
        initial_endtime=[inputs.initial_endtime,'.'];
        endtime=[inputs.endtime,'.'];
        minor_initial=inputs.minor_initial;
        minor_final=inputs.minor_final;
        major_initial=inputs.major_initial;
        major_final=inputs.major_final;
        restart_initial=inputs.restart_initial;
        restart_final=inputs.restart_final;
        initial_cond=inputs.initial_cond;
        timestep_control=inputs.timestep_cntrl;
        nodalizationType=inputs.nodalizationType;  % 1=single tube, 0=annulus

        %Vertical sectioning
        unit_vertical_height=0.08;      % [m]

        %Condensing tube geometry
        condenser_length=1.2;           % [m]
        adiabatic_part_of_condenser=0.4;% [m]
        tube_inner_diam=0.02;           % [m]
        tube_outer_diam=0.03;           % [m]
        cond_annulus_thick=0.002;       % [m]  %condensing pipe is subdivided into inner pipe element and outer annulus

        %heater tank geometry
        heater_tank_length=0.64;        % [m]
        heater_init_water_level=0.32;   % [m]
        heater_diam_inner=0.0837;       % [m]
        heater_diam_outer=0.0889;       % [m]
        
        %feedback line present and what type
        feedback=inputs.feedback;

        %"nodalization" of geometry
        amount_of_heater_full=heater_init_water_level/unit_vertical_height;
        amount_of_heater_empty=(heater_tank_length-heater_init_water_level)/unit_vertical_height;
        amount_of_tube_parts=condenser_length/unit_vertical_height;
        amount_of_heatexchange_parts=(condenser_length-adiabatic_part_of_condenser)/unit_vertical_height;
        amount_of_adiabatic_parts=(adiabatic_part_of_condenser/(condenser_length/amount_of_tube_parts));
        if nodalizationType
            condenser_horizontal_cells=1;  %XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        else
            condenser_horizontal_cells=2;
        end

        heater_full_vol_no=110000000; %starting volume
        heater_full_vol_no_last=heater_full_vol_no+10000*amount_of_heater_full; % last volume
        heater_empty_vol_no=112000000;  %starting volume
        heater_empty_vol_no_last=heater_empty_vol_no+10000*amount_of_heater_empty;  % last volume
        condenser_vol_no=120000000;
        horz_junct_vol_no=130000000;
        coolant_vol=155000000;

        coolant_last_vol=num2str(coolant_vol+10000*amount_of_heatexchange_parts+2);

        %coolant jacket geometry
        jacket_diam_inner=0.1091;       % [m]
        jacket_diam_outer=0.1143;       % [m]        
                
        %Properties
        molar_mass_h2o=18.01528;        % [g/mol]
        molar_mass_He=4.0026;           % [g/mol]
        molar_mass_N2=28;               % [g/mol]


    %% Pre-test calculations - general

        %condensing tube

            %condenser_vol=pi*tube_inner_diam*condenser_length; % [m^3]
            wall_thickness=(tube_outer_diam-tube_inner_diam)/2;
            cond_annulus_hydr_d=num2str(2*cond_annulus_thick);     % [m]
            in_column_diam=tube_inner_diam-2*cond_annulus_thick;   % [m]
            cond_annulus_area=num2str(pi*((tube_inner_diam/2)^2-(in_column_diam/2)^2));  % [m2]
            feedback_line_area=num2str(pi*((tube_inner_diam/2)^2-(in_column_diam/2)^2));  % [m2]
             
            if nodalizationType  %if single columne else if annulus
                        in_column_area=num2str(pi*(tube_inner_diam/2)^2);     % [m]
            else
                        in_column_area=num2str(pi*(in_column_diam/2)^2);     % [m]
            end
                      
            in_column_unit_side_area=num2str(pi*in_column_diam*unit_vertical_height);  %[m2]
            in_column_side_hydrD=2*pi*in_column_diam*unit_vertical_height/(pi*in_column_diam+unit_vertical_height);  % 2*a*b/(a+b), a=pi*in_column_diam, b=unit_vertical_height

        %heater
            heater_area=pi*heater_diam_outer*heater_init_water_level;
%             heater_area=pi*heater_diam_outer*unit_vertical_height;       % for only bottom heating
            heater_cross_section=pi*(heater_diam_inner/2)^2;
            radius_heater_inner=num2str(heater_diam_inner/2);
            radius_heater_outer=num2str(heater_diam_outer/2);
            
            %% POWER POWER POWER POWER POWER POWER POWER POWER POWER POWER POWER POWER POWER POWER POWER POWER
            Heatflux=(Power/heater_area);
            % POWER POWER POWER POWER POWER POWER POWER POWER POWER POWER POWER POWER POWER POWER POWER POWER
            %%
            
            %change pressure from bar to Pa
            Pps=Pps*1e5;
            Pss=Pss*1e5;

        %geometry nodalization
        % num2str is important when writing values with fprintf
        
            if nodalizationType  %if single columne else if annulus
                  condenser_starting=num2str(120010000+10000*amount_of_adiabatic_parts);    
            else
                  condenser_starting=num2str(121010000+10000*amount_of_adiabatic_parts);      
            end
            
            radius_inner=num2str(tube_inner_diam/2);
            radius_outer=num2str(tube_outer_diam/2);

            unit_vertical_height=num2str(unit_vertical_height);


       %change part numbering to strings
            heater_full_minus_one=num2str(amount_of_heater_full-1);
            heater_empty_minus_one=num2str(amount_of_heater_empty-1);
            amount_of_heater_full_parts=num2str(amount_of_heater_full);
            amount_of_heater_full_parts_but_one=num2str(amount_of_heater_full-1);  % FOR HEATING ONLY AT THE BOTTOM
            amount_of_heater_empty_parts=num2str(amount_of_heater_empty);


            tube_minus_one=num2str(amount_of_tube_parts-1);
            coolant_water_minus_one=num2str(amount_of_heatexchange_parts-1);
            amount_of_heatexchange_parts=num2str(amount_of_heatexchange_parts);
            amount_of_tube_parts=num2str(amount_of_tube_parts);

            adiabatic_minus_one=num2str(amount_of_adiabatic_parts-1);
            amount_of_adiabatic_parts=num2str(amount_of_adiabatic_parts);
            heat_str_hydr_D_inner=num2str(tube_inner_diam);
            heat_str_hydr_D_outer=num2str((jacket_diam_inner-tube_outer_diam)*(jacket_diam_inner+tube_outer_diam)/tube_outer_diam);  % from RELAP manual: 4*flow_area/heated_perimeter

       %cooling jacket
            radius_jacket_inner=num2str(jacket_diam_inner/2);
            radius_jacket_outer=num2str(jacket_diam_outer/2);
            cooling_cross_section=num2str(pi*((jacket_diam_inner/2)^2-(tube_outer_diam/2)^2));
            cooling_hydr_d=num2str(jacket_diam_outer-jacket_diam_inner);
            jacket_diam_inner_string=num2str(jacket_diam_inner);
            jacket_diam_outer_string=num2str(jacket_diam_outer);
            cross_section_jacket=num2str(pi*(jacket_diam_inner/2)^2);

 
            disp('Number of input decks:');
            disp(fileCounter)

        %write down nodalization for storing
            nodalization{1,1}='amount_of_heater_full';
            nodalization{2,1}='amount_of_heater_empty';
            nodalization{3,1}='amount_of_tube_parts';
            nodalization{4,1}='amount_of_coolant_water';
            nodalization{5,1}='condenser_horizontal_cells';
            nodalization{6,1}='amount_of_adiabatic_in_condenser';
            nodalization{7,1}='unit_vertical_height';

            nodalization{1,2}=amount_of_heater_full;
            nodalization{2,2}=amount_of_heater_empty;
            nodalization{3,2}=str2double(amount_of_tube_parts);
            nodalization{4,2}=str2double(amount_of_heatexchange_parts);
            nodalization{5,2}=condenser_horizontal_cells;
            nodalization{6,2}=str2double(amount_of_adiabatic_parts);
            nodalization{7,2}=str2double(unit_vertical_height);

            nod_size=size(nodalization);

            for nod_counter=1:nod_size(1)
                if abs(nodalization{nod_counter,2}-round(nodalization{nod_counter,2}))<0.001 && ~strcmp(nodalization{nod_counter,1},'unit_vertical_height')
                    nodalization{nod_counter,2}=round(nodalization{nod_counter,2});
                elseif strcmp(nodalization{nod_counter,1},'unit_vertical_height')   
                else
                    error('Non integer nodalization')
                end
            end
            disp('Nodalization succesful - good job!')

    %% INPUT DECK GENERATION

    for inputDecks_counter=1:fileCounter
        
        %get values for current loop iteration
        curr_Pps=Pps(inputDecks_counter);
        if ~NCmodelFlag
            curr_NC=NC(inputDecks_counter); % sum of N2 init and He init mole fractions
        else
            curr_NC=0;
        end
        
        curr_Helium=Helium(inputDecks_counter); % He init mole fraction
        curr_Tss=Tss(inputDecks_counter)+273.15;
        curr_Pss=Pss(inputDecks_counter);
        curr_Heatflux=Heatflux(inputDecks_counter);       
        curr_Mflowss=Mflowss(inputDecks_counter);
        curr_Power=Power(inputDecks_counter);
            
        %calculate initial temperatures
        %get saturation conditions (divide by 1e6 to change from bar to Mpa)
        Tsat_p = IAPWS_IF97('Tsat_p',curr_Pps/1e6*(1-curr_NC));
        % in case pressure is too low for IAPWS, reduce NC mole fraction by 0.01 % and try again and again and again...
        isnancounter=1;
        while isnan(Tsat_p)
            Tsat_p = IAPWS_IF97('Tsat_p',curr_Pps/1e6*(1-curr_NC*(1-0.0001*isnancounter)));
            isnancounter=isnancounter+1;
        end
        
        % assume saturated steam conditions
        curr_Tps=Tsat_p;  
                
        % define temperature of heater wall
        superheat=1;       % in degrees
        Theater=num2str(Tsat_p+superheat);  
        
        %% calculate NC gases - deck specific, for when initial conditions in vol 120 and vol 121 use ebt=006
        %NC content 
        h2o_mole_fraction=1-curr_NC;
        Helium_mole_fraction=curr_Helium;
        Nitrogen_mole_fraction=curr_NC-curr_Helium;

        %NC masses
        avg_molar_mass_mixture=h2o_mole_fraction*molar_mass_h2o+Helium_mole_fraction*molar_mass_He+Nitrogen_mole_fraction*molar_mass_N2;
        MASS_fraction_h2o=molar_mass_h2o/avg_molar_mass_mixture*h2o_mole_fraction;    %wi = xi*(Mi/M), check wikipedia on mole fracion to mass fraction conversion
        MASS_fraction_Nitrogen=molar_mass_N2/avg_molar_mass_mixture*Nitrogen_mole_fraction;
        MASS_fraction_Helium=molar_mass_He/avg_molar_mass_mixture*Helium_mole_fraction;
        MASS_fraction_NC_total=MASS_fraction_Nitrogen+MASS_fraction_Helium;


        clear uVsat uLsat
        % specific internal energy calculation
        % from Xsteam
        uLsat = 1000*XSteam('uL_p',curr_Pps/1e5*(h2o_mole_fraction)); % [J/kg] saturated liquid specific energy, input in Bar
%         uLsat = 1000*XSteam('uL_p',curr_Pps/1e5);
        uVsat_h2o = 1000*XSteam('uV_p',curr_Pps/1e5*(h2o_mole_fraction)); % [J/kg] saturated vapor specific energy, input in Bar
%         uVsat_h2o = 1000*XSteam('uV_p',curr_Pps/1e5);
        uVsat_N2=5/2*8.314*curr_Tps/molar_mass_N2*1000; % [J/kg], using ideal gas equation for di atomic gases
        uVsat_He=3/2*8.314*curr_Tps/molar_mass_He*1000; % [J/kg], using ideal gas equation for mono atomic gases
        uVsat=uVsat_h2o*MASS_fraction_h2o+uVsat_N2*MASS_fraction_Nitrogen+uVsat_He*MASS_fraction_Helium; % specific internal energy of mixture Engineering Thermodynamics, pg 378
        % https://books.google.ch/books?id=MyHZAgAAQBAJ&pg=PA378&lpg=PA378&dq=specific+internal+energy+mixture&source=bl&ots=WhuzuoEpau&sig=x05hyJUzkTcBMHLgID3ho-JsZO8&hl=en&sa=X&ved=0CB4Q6AEwAGoVChMI09Co493TyAIVSZEsCh3tTAzO#v=onepage&q=specific%20internal%20energy%20mixture&f=false
        %                             uVsat=num2str(uVsat-1000000);
        if curr_NC > 0
            uVsat=uVsat*(1+0.5*curr_NC);
        end
        
        %NC mixture composition
        He_massFr_NCmixtr=MASS_fraction_Helium/MASS_fraction_NC_total;
        N2_massFr_NCmixtr=MASS_fraction_Nitrogen/MASS_fraction_NC_total;
       
        %% convert values to strings and make sure they contain decimal points - required for RELAP to work

        PrimaryPressure=num2str(curr_Pps);
        PrimaryTemp=num2str(curr_Tps);
        SecondaryPressure=num2str(curr_Pss);
        SecondaryTemp=num2str(curr_Tss);
        Heat_flux_strng=num2str(curr_Heatflux);

        He_massFr_NCmixtr=num2str(He_massFr_NCmixtr);  % 01.09.2017
        N2_massFr_NCmixtr=num2str(N2_massFr_NCmixtr); % 01.09.2017
        MASS_fraction_NC_total=num2str(MASS_fraction_NC_total);
        
        Mflow_secondary_seconds=num2str(curr_Mflowss/3600);
        uVsat=num2str(uVsat);
        uLsat=num2str(uLsat);
        
        if isempty(strfind(PrimaryPressure,'.'))
            PrimaryPressure(end+1)='.';
        end   
        
        if isempty(strfind(PrimaryTemp,'.'))
            PrimaryTemp(end+1)='.';
        end   
        
        if isempty(strfind(SecondaryPressure,'.'))
            SecondaryPressure(end+1)='.';
        end  
        
        if isempty(strfind(SecondaryTemp,'.'))
            SecondaryTemp(end+1)='.';
        end   
        
        if isempty(strfind(He_massFr_NCmixtr,'.'))
            He_massFr_NCmixtr(end+1)='.';
        end
        
        if isempty(strfind(N2_massFr_NCmixtr,'.'))
            N2_massFr_NCmixtr(end+1)='.';
        end
        
        if isempty(strfind(MASS_fraction_NC_total,'.'))
            MASS_fraction_NC_total(end+1)='.';
        end  
               
        if isempty(strfind(uLsat,'.'))
            uLsat(end+1)='.';
        end
        
        if isempty(strfind(uVsat,'.'))
            uVsat(end+1)='.';
        end

        if isempty(strfind(heat_str_hydr_D_inner,'.'))
            heat_str_hydr_D_inner(end+1)='.';
        end                 
               
        
        %% generate file name and path to file
        if input_type
            %prepare components for naming
            
            % remove dots from strings
            unit_vert_h_name=unit_vertical_height;
            unit_vert_h_name(unit_vert_h_name=='.')='d'; 
            
            Helium_content_name=He_massFr_NCmixtr;
            Helium_content_name(Helium_content_name=='.')='';  %removes dots from string, so it can be used for file name
            
            NC_gas_name=MASS_fraction_NC_total;
            NC_gas_name(NC_gas_name=='.')='-';  %removes dots from string, so it can be used for file name
                    
            PrimaryPressure_name=num2str(curr_Pps/1e5);  %convert to bar
            PrimaryPressure_name(PrimaryPressure_name=='.')='-';
        
            Mflow_secondary_hours_name=num2str(curr_Mflowss);
            %remove dots from Mflow...
            Mflow_secondary_hours_name(Mflow_secondary_hours_name=='.')='-';
            
            PrimaryTemp_name=num2str(floor(curr_Tps));
            Tss_name=num2str(floor(curr_Tss));
        
            % remove dots from power
            Power_name=num2str(curr_Power);
            Power_name(Power_name=='.')='-';
            
            deckDescription=[PrimaryPressure_name,'_',PrimaryTemp_name,'_',NC_gas_name,'_',Helium_content_name,'_',Power_name,'_',Tss_name,'_',Mflow_secondary_hours_name,'_',unit_vert_h_name,'_',num2str(condenser_horizontal_cells)];                      %save file name WITHOUT extension
        else
            deckDescription=fileList{inputDecks_counter};
            deckDescription=deckDescription(1:strfind(deckDescription,'_RELAP')-1);   %removes _INPUT from the string
        end

        %% WRITE INPUT DECK FILE 
        
        %create directory if does not exist
        storagePath=[file_dir,'\',deckDescription,'_RELAP'];
        
        if ~exist(storagePath,'file')
            mkdir(storagePath)
        end             
        
        %store description of nodalization
        nodalizationFile=[storagePath,'\nodalization'];
        save(nodalizationFile,'nodalization');
        
        %INPUT DECK
        fid = fopen([storagePath,'\',deckDescription,'_input_deck.i'], 'wt'); %open the file

        %                             heatstr1Temp=num2str(floor((Tps+Tss(Superheat_count))/2));                      %assume heatstructure temp to be an average of PS/SS temperatures [K]

        %input file txt
        fprintf(fid,'* title card\n'); 
        fprintf(fid,'= simple pipe 1 + heat structures\n'); 
        fprintf(fid,'*================================================================\n'); 
        fprintf(fid,'* problem type   option\n'); 
        fprintf(fid,'100       new    transnt\n'); 
        fprintf(fid,'*================================================================\n'); 
        fprintf(fid,'* input / output units\n'); 
        fprintf(fid,'102 si *optional, default are si units\n'); 
        fprintf(fid,'* Restart-Plot Control Card\n'); 
        fprintf(fid,'104 ncmpress\n'); 
        fprintf(fid,'*================================================================\n'); 
        if ~curr_NC==0
            fprintf(fid,'*define non condensable gases\n');
           fprintf(fid,'110 helium nitrogen\n');
%             fprintf(fid,'110 hydrogen nitrogen\n');
            fprintf(fid,'*================================================================\n');
            fprintf(fid,'*define non condensable gases MASS FRACTIONS\n');
            fprintf(fid,'*    helium  nitrogen\n');
            fprintf(fid,'115 %s      %s\n',He_massFr_NCmixtr, N2_massFr_NCmixtr);                                   %****************************************************
            fprintf(fid,'*================================================================\n');
        end
        fprintf(fid,'* Initial Time Value\n');
        fprintf(fid,'200 0.\n');
        fprintf(fid,'*================================================================\n');
        fprintf(fid,'* time step control card\n');
        fprintf(fid,'*   endtime min.dt max.dt control minor major restart\n');
%         inputDecks_counter
        fprintf(fid,'201 %s      %s     %s     %s   %s    %s    %s\n', initial_endtime, mindt, initial_maxdt, timestep_control, minor_initial, major_initial,restart_initial); %****************************************************
        fprintf(fid,'202 %s      %s     %s     %s   %s    %s    %s\n', endtime, mindt, final_maxdt, timestep_control, minor_final, major_final,restart_final); %****************************************************
        fprintf(fid,'*================================================================\n');
        fprintf(fid,'* extra variables to print\n');
        fprintf(fid,'*         var param\n');
        % for printing extra variables in heater and condenser
        card_no=20800001;     
        
        extra_parameters_to_print={'tmassv','gammac','gammai','qualhy','vvol','vollev'};
        
        %loop through all extra parameters
        for ext_prm_cntr=1:numel(extra_parameters_to_print)
            if strcmp(extra_parameters_to_print{ext_prm_cntr},'avol')
                curr_heater_full_vol_no=heater_full_vol_no;  
                curr_heater_empty_vol_no=heater_empty_vol_no;
            else
                curr_heater_full_vol_no=heater_full_vol_no;
                curr_heater_empty_vol_no=heater_empty_vol_no;
            end
%             condenser_vol_no_ctr=condenser_vol_no;
            
%            loop through full heater volume
            for heater_volume_counter=1:str2double(amount_of_heater_full_parts)
                curr_heater_full_vol_no=curr_heater_full_vol_no+10000;
                fprintf(fid,'%s  %s %s \n', num2str(card_no),extra_parameters_to_print{ext_prm_cntr}, num2str(curr_heater_full_vol_no));
                card_no=card_no+1;                                
            end
            % loop through empty heater volume
            for heater_volume_2_counter=1:str2double(amount_of_heater_empty_parts)
                curr_heater_empty_vol_no=curr_heater_empty_vol_no+10000;
                fprintf(fid,'%s  %s %s \n', num2str(card_no),extra_parameters_to_print{ext_prm_cntr}, num2str(curr_heater_empty_vol_no));
                card_no=card_no+1;
            end
            
            % loop through tube heater volume
            
            for horz_cell_counter=1:condenser_horizontal_cells
                    condenser_vol_no_ctr=condenser_vol_no+1000000*(horz_cell_counter-1);
                for condenser_volume_counter=1:str2double(amount_of_tube_parts)
                    condenser_vol_no_ctr=condenser_vol_no_ctr+10000;

                    if strcmp(extra_parameters_to_print{ext_prm_cntr},'avol')
                        fprintf(fid,'%s  %s %s \n', num2str(card_no),extra_parameters_to_print{ext_prm_cntr}, num2str(condenser_vol_no_ctr+3)); % +3 points to a side wall
                    else
                        fprintf(fid,'%s  %s %s \n', num2str(card_no),extra_parameters_to_print{ext_prm_cntr}, num2str(condenser_vol_no_ctr));
                    end    
                    
                    card_no=card_no+1;
                end
            end
        end
        
        %water level in whole system
%         fprintf(fid,'%s  %s %s \n', num2str(card_no),'levhgt', '000'); %illeagal   

        %add extra flows to important junctions
%         fprintf(fid,'%s  %s %s \n', num2str(card_no),'qualaj', num2str(111000000));
%         fprintf(fid,'%s  %s %s \n', num2str(card_no+1),'qualaj', num2str(111000000));
%         fprintf(fid,'%s  %s %s \n', num2str(card_no),'mflowfj', num2str(111000000));
%         fprintf(fid,'%s  %s %s \n', num2str(card_no+1),'mflowgj', num2str(111000000));
%         fprintf(fid,'%s  %s %s \n', num2str(card_no+2),'mflowfj', num2str(115000000));
%         fprintf(fid,'%s  %s %s \n', num2str(card_no+3),'mflowgj', num2str(115000000));
%         fprintf(fid,'%s  %s %s \n', num2str(card_no+4),'mflowfj', num2str(116000000));
%         fprintf(fid,'%s  %s %s \n', num2str(card_no+5),'mflowgj', num2str(116000000));        
        
        fprintf(fid,'* component data\n');
        fprintf(fid,'*----------------------------------------------------------------\n');

    %% ******************** heater
        fprintf(fid,'*-----------------------------------------------------------------INNER TEST PIPE\n');

        fprintf(fid,'* component 110 - full heater\n');
        fprintf(fid,'*        name   type\n');
        fprintf(fid,'1100000  pipe  pipe\n');
        fprintf(fid,'*        no.volumes\n');
        fprintf(fid,'1100001  %s \n', amount_of_heater_full_parts);  
        fprintf(fid,'*        area                             vol.no.\n');
        fprintf(fid,'1100101  %s                               %s \n', heater_cross_section, amount_of_heater_full_parts);
        fprintf(fid,'*        length                           vol.no.\n');
        fprintf(fid,'1100301  %s                               %s \n', unit_vertical_height, amount_of_heater_full_parts);
        fprintf(fid,'*        v-ang                            vol.no.\n');
        fprintf(fid,'1100601  90.                              %s \n', amount_of_heater_full_parts);
        fprintf(fid,'*        rough   dhy                      vol.no.\n');
        fprintf(fid,'1100801  0.      0.                       %s \n', amount_of_heater_full_parts);
        fprintf(fid,'*        tlpvbfe                          vol.no.\n');
        fprintf(fid,'1101001  0001000                          %s \n', amount_of_heater_full_parts);
        fprintf(fid,'*        efvcahs                          jun.no.\n');
        fprintf(fid,'1101101  0001000                          %s \n', heater_full_minus_one);              
        %                             fprintf(fid,'*        ebt  temperature  stat_qual       vol.no.\n');
        %                             fprintf(fid,'1101201  001  %s.      %s          0.  0. 0.   %s \n', PrimaryTemp,MASS_fraction_NC_total, amount_of_heater_full_parts); %***************************************************
        fprintf(fid,'*        ebt  pressure  stat_qual         vol.no.\n');
        fprintf(fid,'1101201  002  %s    0.     0.  0.  0.     %s \n',PrimaryPressure, amount_of_heater_full_parts); %****************************************************
        %                             fprintf(fid,'*        ebt  pressure temperature        vol.no.\n');
        %                             fprintf(fid,'1101201  003  %s.      %s          0.  0. 0.   %s \n',PrimaryPressure, Theater, amount_of_heater_full_parts); %***************************************************
        %                             fprintf(fid,'*        ebt  pressure temperature  stat_qual       vol.no.\n');
        %                             fprintf(fid,'1101201  004  %s.      %s.          0.01  0. 0.   %s \n',PrimaryPressure, PrimaryTemp, amount_of_heater_full_parts); %***************************************************
        fprintf(fid,'*        mass flow (=1)\n');
        fprintf(fid,'1101300  1\n');
        fprintf(fid,'*        flowf  flowg      velj           jun.no.\n');
        fprintf(fid,'1101301  0.     0.         0.             %s \n', heater_full_minus_one);  %****************************************************

        fprintf(fid,'*----------------------------------------------------------------\n');
        fprintf(fid,'* component 111 - heater junction\n');
        fprintf(fid,'*        name   type\n');
        fprintf(fid,'1110000  injun  sngljun\n');
        fprintf(fid,'*        from       to         area  floss rloss  jefvcahs\n');
        fprintf(fid,'1110101  %s  112010001  %s    0.    0.     00000000\n', num2str(heater_full_vol_no_last+2), heater_cross_section);    %*************************************************************
        fprintf(fid,'*        junctionD  CCFL  gasintercept     slope\n');
        fprintf(fid,'1110110  0.         0.        1.               1.\n');
        fprintf(fid,'*        ctl  velflowf  velflowg     interface velocity\n');
        fprintf(fid,'1110201  0.   0.       0.             0.\n');             %*********************

        fprintf(fid,'*----------------------------------------------------------------\n');
        fprintf(fid,'* component 112 - empty heater\n');
        fprintf(fid,'*        name   type\n');
        fprintf(fid,'1120000  pipe  pipe\n');
        fprintf(fid,'*        no.volumes\n');
        fprintf(fid,'1120001  %s \n', amount_of_heater_empty_parts);  
        fprintf(fid,'*        area                             vol.no.\n');
        fprintf(fid,'1120101  %s                               %s \n', heater_cross_section, amount_of_heater_empty_parts);
        fprintf(fid,'*        length                           vol.no.\n');
        fprintf(fid,'1120301  %s                               %s \n', unit_vertical_height, amount_of_heater_empty_parts);
        fprintf(fid,'*        v-ang                            vol.no.\n');
        fprintf(fid,'1120601  90.                              %s \n', amount_of_heater_empty_parts);
        fprintf(fid,'*        rough   dhy                      vol.no.\n');
        fprintf(fid,'1120801  0.      0.                       %s \n', amount_of_heater_empty_parts);
        fprintf(fid,'*        tlpvbfe                          vol.no.\n');
        fprintf(fid,'1121001  0001000                          %s \n', amount_of_heater_empty_parts);
        fprintf(fid,'*        efvcahs                          jun.no.\n');
        fprintf(fid,'1121101  0001000                          %s \n', heater_empty_minus_one);       
        if curr_NC== 0                            
        %                             fprintf(fid,'*        ebt  temperature  stat_qual       vol.no.\n');
        %                             fprintf(fid,'1121201  001  %s.      %s          0.  0. 0.   %s \n', PrimaryTemp,MASS_fraction_NC_total, amount_of_heater_empty_parts); %***************************************************
            fprintf(fid,'*        ebt  pressure  stat_qual         vol.no.\n');
            fprintf(fid,'1121201  002  %s    1.     0.  0.  0.     %s \n',PrimaryPressure, amount_of_heater_empty_parts); %****************************************************
        %                             fprintf(fid,'*        ebt  pressure temperature        vol.no.\n');
        %                             fprintf(fid,'1121201  003  %s.      %s          0.  0. 0.   %s \n',PrimaryPressure, num2str(Tps), amount_of_heater_empty_parts); %***************************************************
        else
            if initial_cond==1
                fprintf(fid,'*        ebt press temp stat_qual vol.no.\n');
                fprintf(fid,'1121201  004 %s    %s   1.   0. 0.   %s \n',PrimaryPressure, PrimaryTemp, amount_of_heater_empty_parts); %***************************************************
            elseif initial_cond==2
                fprintf(fid,'*        ebt press stat_qual NC_fraction   vol.no.\n');
                fprintf(fid,'1121201  005 %s  0.99999999 %s 0.0 0.0 %s \n',PrimaryTemp, MASS_fraction_NC_total, amount_of_heater_empty_parts);
            elseif initial_cond==3 
                fprintf(fid,'*        ebt press liq_int_en gas_int_en gas_void NC_quality   vol.no.\n');
                fprintf(fid,'1121201  006 %s  %s %s 1.0 %s %s \n',PrimaryPressure,uLsat,uVsat, MASS_fraction_NC_total, amount_of_heater_empty_parts); %***************************************************
                
            end
        end                            
        fprintf(fid,'*        mass flow (=1)\n');
        fprintf(fid,'1121300  1\n');
        fprintf(fid,'*        flowf  flowg  velj  jun.no.\n');
        fprintf(fid,'1121301  0.     0.     0.    %s \n', heater_empty_minus_one);  %************

    %% water feedback line - experimental feature
        if feedback==3 
            % water feedback line volume
            fprintf(fid,'*----------------------------------------------------------------\n');
            fprintf(fid,'* component 113 - water feedback line\n');
            fprintf(fid,'*        name   type\n');
            fprintf(fid,'1130000  h2ofeedb  snglvol\n');
            fprintf(fid,'*        area   length  volume  h-ang  v-ang  delz   rough  dhy  tlpvbfe\n');
            fprintf(fid,'1130101  %s     %s      0.      0.     90.    %s     0.     0    0110010\n',feedback_line_area,num2str(heater_init_water_level),num2str(heater_init_water_level));
            fprintf(fid,'*        ebt  pressure  stat_qual \n');
            fprintf(fid,'1130200  002  %s    1.     0.  \n',PrimaryPressure); %****************************************************

            % junction between water feedback and boiler tank at the bottom
            fprintf(fid,'*----------------------------------------------------------------\n');
            fprintf(fid,'* component 114 - junction boiler feedback BOTTOM\n');
            fprintf(fid,'*        name       type\n');
            fprintf(fid,'1140000  h2ofbjun  sngljun\n');
            fprintf(fid,'*        from       to         area  floss rloss  jefvcahs\n');         
            fprintf(fid,'1140101  113010001 110010001 %s    1.    1.     00100000\n', feedback_line_area); %,num2str(heater_empty_vol_no_last+2), cond_annulus_area);    %*************************************************************
            fprintf(fid,'*        junctionD  flooding  gasintercept slope\n');
            fprintf(fid,'1140110  0.         0.        1.           1.\n');
            fprintf(fid,'*        ctl  velflowf  velflowg     interface velocity\n');
            fprintf(fid,'1140201  0.   0.       0.            0.\n');             %******************

            % junction between water feedback and boiler tank at the top 
            fprintf(fid,'*----------------------------------------------------------------\n');
            fprintf(fid,'* component 117 - junction boiler feedback TOP\n');
            fprintf(fid,'*        name       type\n');
            fprintf(fid,'1170000  h2ofbjun  sngljun\n');
            fprintf(fid,'*        from       to         area  floss rloss  jefvcahs\n');         
            fprintf(fid,'1170101  %s 113010002 %s    1.    1.     00100000\n',num2str(heater_empty_vol_no+10001), feedback_line_area); 
            fprintf(fid,'1170110  0.         0.        1.           1.\n');
            fprintf(fid,'*        ctl  velflowf  velflowg     interface velocity\n');
            fprintf(fid,'1170201  0.   0.       0.            0.\n');             %******************
        end
        
        
    %% condensing tube
        
        % junction to inner pipe
        fprintf(fid,'*----------------------------------------------------------------\n');
        fprintf(fid,'* component 115 - inlet junction to inner pipe\n');
        fprintf(fid,'*        name       type\n');
        fprintf(fid,'1150000  inpipe sngljun\n');
        fprintf(fid,'*        from       to        area  floss rloss  jefvcahs  \n');
        fprintf(fid,'1150101  %s  120010001 %s    1.    1.     00100100 \n',num2str(heater_empty_vol_no_last+2), in_column_area); %, condeser_unit_side_area);    %**************************
        fprintf(fid,'*        junctionD flooding gasintercept slope\n');
        fprintf(fid,'1150110  0.        0.       1.           1.\n');
        fprintf(fid,'*        ctl  velflowf  velflowg     interface velocity\n');
        fprintf(fid,'1150201  0.   0.       0.            0.\n');             %******************

        if ~nodalizationType  % if there is annulus
            %temp volume for water feedback
            
            if feedback==2
                fprintf(fid,'* component 113 - temp water feedback\n');
                fprintf(fid,'*        name   type\n');
                fprintf(fid,'1130000  h2ofeedb  snglvol\n');
                fprintf(fid,'*        area   length  volume  h-ang  v-ang  delz   rough  dhy  tlpvbfe\n');
                fprintf(fid,'1130101  %s     %s      0.      0.     90.    %s     0.     0    0110010\n',cond_annulus_area,num2str(heater_tank_length),num2str(heater_tank_length));
                fprintf(fid,'*        ebt  pressure  stat_qual \n');
                fprintf(fid,'1130200  002  %s    1.     0.  \n',PrimaryPressure); %****************************************************

                % junction between water feedback and boiler tank
                fprintf(fid,'*----------------------------------------------------------------\n');
                fprintf(fid,'* component 114 - inlet junction to annulus\n');
                fprintf(fid,'*        name       type\n');
                fprintf(fid,'1140000  h2ofbjun  sngljun\n');
                fprintf(fid,'*        from       to         area  floss rloss  jefvcahs\n');         
                fprintf(fid,'1140101  113010001 110010001 %s    1.    1.     00100000\n', cond_annulus_area); %,num2str(heater_empty_vol_no_last+2), cond_annulus_area);    %*************************************************************
                fprintf(fid,'*        junctionD  flooding  gasintercept slope\n');
                fprintf(fid,'1140110  0.         0.        1.           1.\n');
                fprintf(fid,'*        ctl  velflowf  velflowg     interface velocity\n');
                fprintf(fid,'1140201  0.   0.       0.            0.\n');             %******************
            end
                            
                            
            % junction to annulus
            fprintf(fid,'*----------------------------------------------------------------\n');
            fprintf(fid,'* component 116 - inlet junction to annulus\n');
            fprintf(fid,'*        name       type\n');
            fprintf(fid,'1160000  inannu  sngljun\n');
            fprintf(fid,'*        from       to         area  floss rloss  jefvcahs\n');
            if feedback==2
                fprintf(fid,'1160101  121010001 %s %s    1.    1.     00100000\n',num2str(113010002), cond_annulus_area); %,num2str(heater_empty_vol_no_last+2), cond_annulus_area);    %*************************************************************
            else            
                fprintf(fid,'1160101  121010001 %s %s    1.    1.     00100100\n',num2str(heater_empty_vol_no_last+2), cond_annulus_area);     %*********
            end
            fprintf(fid,'*        junctionD  flooding  gasintercept slope\n');
            fprintf(fid,'1160110  0.         0.        1.           1.\n');
            fprintf(fid,'*        ctl  velflowf  velflowg     interface velocity\n');
            fprintf(fid,'1160201  0.   0.       0.            0.\n');             %******************
        end

        %******************** inner pipe / column (adiabatic part + condenser)

        fprintf(fid,'*----------------------------------------------------------------\n');
        fprintf(fid,'* component 120 inner column\n');
        fprintf(fid,'*        name    type\n');
        fprintf(fid,'1200000  inpipe  pipe\n');
        fprintf(fid,'*        no.volumes\n');
        fprintf(fid,'1200001  %s \n', amount_of_tube_parts);  
        fprintf(fid,'*        area                             vol.no.\n');
        fprintf(fid,'1200101  %s                               %s \n', in_column_area, amount_of_tube_parts);      
                % testing extra y and z coordingates
%                     fprintf(fid,'*        length                           vol.no.\n');% TEST
%                     fprintf(fid,'1201601  %s                               %s \n', in_column_area, amount_of_tube_parts);   % TEST
%                     fprintf(fid,'*        length                           vol.no.\n'); % TEST
%                     fprintf(fid,'1201701  %s                               %s \n', in_column_area, amount_of_tube_parts); % TEST        
                % up to here
        fprintf(fid,'*        length                           vol.no.\n');
        fprintf(fid,'1200301  %s                               %s \n', unit_vertical_height, amount_of_tube_parts);
                % XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
%                     fprintf(fid,'*        length                           vol.no.\n'); % TEST
%                     fprintf(fid,'1201801  %s                               %s \n', unit_vertical_height, amount_of_tube_parts); % TEST
%                     fprintf(fid,'*        length                           vol.no.\n'); % TEST
%                     fprintf(fid,'1201901  %s                               %s \n', unit_vertical_height, amount_of_tube_parts); % TEST
                % XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX^^^^^^^^^^^^^^^^
        fprintf(fid,'*        v-ang                            vol.no.\n');
        fprintf(fid,'1200601  90.                              %s \n', amount_of_tube_parts);
        fprintf(fid,'*        rough   dhy                      vol.no.\n');
        fprintf(fid,'1200801  0.      0.                       %s \n', amount_of_tube_parts);
                % XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
%                     fprintf(fid,'*        rough   dhy                      vol.no.\n');
%                     fprintf(fid,'1202301  0.      0.                       %s \n', amount_of_tube_parts);
%                     fprintf(fid,'*        rough   dhy                      vol.no.\n');
%                     fprintf(fid,'1202401  0.      0.                       %s \n', amount_of_tube_parts);
%                 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX^^^^^^^^^^^^^^^^
        fprintf(fid,'*        tlpvbfe                          vol.no.\n');
        fprintf(fid,'1201001  0001000                          %s \n', amount_of_tube_parts); 
        fprintf(fid,'*        efvcahs                          jun.no.\n');
        fprintf(fid,'1201101  0001000                          %s \n', tube_minus_one);   
        if curr_NC==0 
                fprintf(fid,'*        ebt  press  stat_qual         vol.no.\n');
                fprintf(fid,'1201201  002  %s     1.     0.  0.  0.     %s \n',PrimaryPressure, amount_of_tube_parts); %****************************************************
        else
                if initial_cond==1
                    fprintf(fid,'*        ebt press temp stat_qual       vol.no.\n');
                    fprintf(fid,'1201201  004 %s    %s  1.  0. 0.   %s \n',PrimaryPressure, PrimaryTemp,amount_of_tube_parts); %*****
                elseif initial_cond==2
                    fprintf(fid,'*        ebt press stat_qual NC_fraction   vol.no.\n');
                    fprintf(fid,'1201201  005 %s  0.99999999 %s 0.0 0.0 %s \n',PrimaryTemp, MASS_fraction_NC_total, amount_of_tube_parts);
                elseif initial_cond==3
                    fprintf(fid,'*       ebt  pressure liq_int_en gas_int_en  gas_void NC_quality   vol.no.\n');
                    fprintf(fid,'1201201 006 %s  %s %s 1.0 %s %s \n',PrimaryPressure,uLsat,uVsat, MASS_fraction_NC_total, amount_of_tube_parts); %***************************************************           
%                     fprintf(fid,'1201201 008 %s  %s %s 1.0 %s %s \n',PrimaryPressure, PrimaryTemp, PrimaryTemp, MASS_fraction_NC_total, amount_of_tube_parts);
                end
        end
        fprintf(fid,'*        mass flow (=1)\n');
        fprintf(fid,'1201300  1\n');
        fprintf(fid,'*        flowf  flowg      velj           jun.no.\n');
        fprintf(fid,'1201301  0.     0.         0.             %s \n', tube_minus_one);  %****************************************************

        if ~nodalizationType  % if there is annulus
            %******************** condensing annulus (adiabatic part + condenser)
            fprintf(fid,'*----------------------------------------------------------------\n');
            fprintf(fid,'* component 121 annulus \n');
            fprintf(fid,'*        name    type\n');
            fprintf(fid,'1210000  annulus annulus\n');
            fprintf(fid,'*        no.volumes\n');
            fprintf(fid,'1210001  %s \n', amount_of_tube_parts);  
            fprintf(fid,'*        area                             vol.no.\n');
            fprintf(fid,'1210101  %s                               %s \n', cond_annulus_area, amount_of_tube_parts);
                     % testing extra y and z coordingates
%                         fprintf(fid,'*        length                           vol.no.\n');% TEST
%                         fprintf(fid,'1211601  %s                               %s \n', cond_annulus_area, amount_of_tube_parts);   % TEST
%                         fprintf(fid,'*        length                           vol.no.\n'); % TEST
%                         fprintf(fid,'1211701  %s                               %s \n', cond_annulus_area, amount_of_tube_parts); % TEST        
                    % up to here
            fprintf(fid,'*        length                           vol.no.\n');
            fprintf(fid,'1210301  %s                               %s \n', unit_vertical_height, amount_of_tube_parts);
                    % XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
%                         fprintf(fid,'*        length                           vol.no.\n'); % TEST
%                         fprintf(fid,'1211801  %s                               %s \n', unit_vertical_height, amount_of_tube_parts); % TEST
%                         fprintf(fid,'*        length                           vol.no.\n'); % TEST
%                         fprintf(fid,'1211901  %s                               %s \n', unit_vertical_height, amount_of_tube_parts); % TEST
                    % XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX^^^^^^^^^^^^^^^^
            fprintf(fid,'*        v-ang                            vol.no.\n');
            fprintf(fid,'1210601  90.                              %s \n', amount_of_tube_parts);
            fprintf(fid,'*        rough   dhy                      vol.no.\n');
            fprintf(fid,'1210801  0.      %s                       %s \n', cond_annulus_hydr_d, amount_of_tube_parts);
                    % XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
%                         fprintf(fid,'*        rough   dhy                      vol.no.\n');
%                         fprintf(fid,'1212301  0.      0.                       %s \n', amount_of_tube_parts);
%                         fprintf(fid,'*        rough   dhy                      vol.no.\n');
%                         fprintf(fid,'1212401  0.      0.                       %s \n', amount_of_tube_parts);
                    % XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX^^^^^^^^^^^^^^^^
            fprintf(fid,'*        tlpvbfe                          vol.no.\n');
            fprintf(fid,'1211001  0000000                          %s \n', amount_of_tube_parts); 
            fprintf(fid,'*        efvcahs                          jun.no.\n');
            fprintf(fid,'1211101  0001000                          %s \n', tube_minus_one);   
            if curr_NC== 0 
                    fprintf(fid,'*        ebt  press  stat_qual         vol.no.\n');
                    fprintf(fid,'1211201  002  %s     1.     0.  0.  0.     %s \n',PrimaryPressure, amount_of_tube_parts); %****************************************************
            else
                    if initial_cond==1
                        fprintf(fid,'*        ebt press temp stat_qual       vol.no.\n');
                        fprintf(fid,'1211201  004 %s    %s  1.  0. 0.   %s \n',PrimaryPressure, PrimaryTemp,amount_of_tube_parts); %*****
                    elseif initial_cond==2
                        fprintf(fid,'*        ebt press stat_qual NC_fraction   vol.no.\n');
                        fprintf(fid,'1211201  005 %s  0.99999999 %s 0.0 0.0 %s \n',PrimaryTemp, MASS_fraction_NC_total, amount_of_tube_parts);  %static quality is limited to two phase
                    elseif initial_cond==3
                        fprintf(fid,'*       ebt  pressure liq_int_en gas_int_en  gas_void NC_quality   vol.no.\n');
%                         fprintf(fid,'1211201 006 %s  %s %s 1.0 %s %s \n',PrimaryPressure,uLsat,uVsat, MASS_fraction_NC_total, amount_of_tube_parts); %***************************************************
                        fprintf(fid,'1211201  005 %s  0.99999999 %s 0.0 0.0 %s \n',PrimaryTemp, MASS_fraction_NC_total, amount_of_tube_parts);
%                         fprintf(fid,'1211201 008 %s  %s %s 1.0 %s %s \n',PrimaryPressure, PrimaryTemp, PrimaryTemp, MASS_fraction_NC_total, amount_of_tube_parts);
                    end
            end
            fprintf(fid,'*        mass flow (=1)\n');
            fprintf(fid,'1211300  1\n');
            fprintf(fid,'*        flowf  flowg      velj           jun.no.\n');
            fprintf(fid,'1211301  0.     0.         0.             %s \n', tube_minus_one);  %****************************************************
            fprintf(fid,'*        jnct_hydr_d CCFL gas_intcpt slope jun.no.\n');
            fprintf(fid,'1211401  %s          0.   1.         1.    %s \n',cond_annulus_hydr_d, tube_minus_one);  %***********

            %********************* horizontal junction between inner column and annulus
            fprintf(fid,'*----------------------------------------------------------------\n');
            fprintf(fid,'*        name     type\n');
            fprintf(fid,'1300000  horzjun  mtpljun\n');
            fprintf(fid,'*        no_of_jun   init_cond_ctrl\n');
            fprintf(fid,'1300001  %s          0\n', amount_of_tube_parts);
            fprintf(fid,'*        from  to area  floss rloss  efvcahs  W7 W8 W9  from_incr to_incr  W12  junction_lim\n');
            fprintf(fid,'1300011  120010004 121010003 %s 0. 0. 0001000 1. 1. 1. 10000 10000 0 %s\n',in_column_unit_side_area, amount_of_tube_parts);   %*************************************************************
            fprintf(fid,'*        initflow_f initflow_g  junction_lim\n');
            fprintf(fid,'1301011  0.         0.          %s\n', amount_of_tube_parts);
            fprintf(fid,'*        hydrD  CCFL gas_intcpt slope junction_lim\n');
                        fprintf(fid,'1302011  %s     0.   1.         1.    %s\n',num2str(in_column_side_hydrD), amount_of_tube_parts);
            %             fprintf(fid,'1302011  %s     0.   1.         1.    %s\n',num2str(in_column_diam), amount_of_tube_parts);            %**
%             fprintf(fid,'1302011  0     0.   1.         1.    %s\n', amount_of_tube_parts);        
        end

        %% COOLANT WATER SIDE
        fprintf(fid,'*----------------------------------------------------------------OUTER COOLING JACKET\n');
        fprintf(fid,'* component 140 - inlet volume\n');
        fprintf(fid,'*        name     type\n');
        fprintf(fid,'1400000  inletC   tmdpvol\n');
        fprintf(fid,'*        area    length  volume  h-ang  v-ang  delz   rough  dhy    tlpvbfe\n');
        fprintf(fid,'1400101  %s      1.      0.      0.     90.    1.     0.     0.     0000000 \n', cooling_cross_section);
        fprintf(fid,'*        ctl\n');
        fprintf(fid,'1400200  003\n');
        fprintf(fid,'*        time  pressure    temperature\n');
        fprintf(fid,'1400201  0.    %s          %s \n',SecondaryPressure, SecondaryTemp);                  %****************************************************
        fprintf(fid,'*----------------------------------------------------------------\n');
        fprintf(fid,'* component 150 - inlet junction\n');
        fprintf(fid,'*        name   type\n');
        fprintf(fid,'1500000  injunC tmdpjun\n');
        fprintf(fid,'*        from       to         area    *     floss rloss  jefvcahs\n');
        fprintf(fid,'1500101  140010002  155010001  %s      *     0.    0.     00000000\n',  cooling_cross_section);
        fprintf(fid,'*        ctl\n');
        fprintf(fid,'1500200  1\n');
        fprintf(fid,'*        time  flowf      flowg  interval velocity\n');
        fprintf(fid,'1500201  0.    0.         0.     0\n');              %****************************************************
        fprintf(fid,'1500202  %s.    %s         0.     0\n',action_start,Mflow_secondary_seconds);              %****************************************************
        fprintf(fid,'*----------------------------------------------------------------\n');
        fprintf(fid,'* component 155 - pipes\n');
        fprintf(fid,'*        name   type\n');
        fprintf(fid,'1550000  pipeC  annulus\n');
        fprintf(fid,'*        no.volumes\n');
        fprintf(fid,'1550001  %s \n',amount_of_heatexchange_parts);
        fprintf(fid,'*        area                             vol.no.\n');
        fprintf(fid,'1550101  %s                               %s\n', cooling_cross_section,amount_of_heatexchange_parts);
        fprintf(fid,'*        length                           vol.no.\n');
        fprintf(fid,'1550301  %s                               %s\n', unit_vertical_height,amount_of_heatexchange_parts);
        fprintf(fid,'*        v-ang                            vol.no.\n');
        fprintf(fid,'1550601  90.                              %s\n',amount_of_heatexchange_parts);
        fprintf(fid,'*        rough   dhy                      vol.no.\n');
        fprintf(fid,'1550801  0.      %s                       %s\n',cooling_hydr_d, amount_of_heatexchange_parts);
        fprintf(fid,'*        tlpvbfe                          vol.no.\n');
        fprintf(fid,'1551001  0000000                          %s\n',amount_of_heatexchange_parts);
        fprintf(fid,'*        efvcahs                          jun.no.\n');
        fprintf(fid,'1551101  0000000                          %s\n',coolant_water_minus_one);  
        fprintf(fid,'*        ebt  pressure  temperature       vol.no.\n');
        fprintf(fid,'1551201  003  %s       %s    0.  0.  0.  %s\n', SecondaryPressure, SecondaryTemp,amount_of_heatexchange_parts);  %****************************************************
        fprintf(fid,'*        mass flow (=1)\n');
        fprintf(fid,'1551300  1\n');
        fprintf(fid,'*        flowf      flowg  velj          jun.no.\n');
        fprintf(fid,'1551301  %s    0.     0.                 %s \n',Mflow_secondary_seconds,coolant_water_minus_one);     %****************************************************
        fprintf(fid,'*----------------------------------------------------------------\n');
        fprintf(fid,'* component 160 - outlet junction\n');
        fprintf(fid,'*        name    type\n');
        fprintf(fid,'1600000  outjunC sngljun\n');
        fprintf(fid,'*        from  to           area fwd. loss  rev. loss   jefvcahs\n');
        fprintf(fid,'1600101  %s    165010001    %s   0.0        0.0         00000000\n',coolant_last_vol, cooling_cross_section);
        fprintf(fid,'*        hydraulic_d  flooding_correlation gas_intercept slope\n');
        fprintf(fid,'1600110  0            0                    1.0           1.0\n');
        fprintf(fid,'*        ctl  flowf      flowg\n');
        fprintf(fid,'1600201  1    %s         0. \n',Mflow_secondary_seconds);                      %****************************************************
        fprintf(fid,'*----------------------------------------------------------------\n');
        fprintf(fid,'* component 165 - outlet volume\n');
        fprintf(fid,'*        name    type\n');
        fprintf(fid,'1650000  outvolC tmdpvol\n');
        fprintf(fid,'*        area    length  volume h-ang v-ang  delz  rough  dhy  tlpvbfe\n');
        fprintf(fid,'1650101  %s      1.      0.     0.    90.    1.    0.     0.   0000000\n', cooling_cross_section);
        fprintf(fid,'*        ctl\n');
        fprintf(fid,'1650200  003\n');
        fprintf(fid,'*        time  pressure    temperature\n');
        fprintf(fid,'1650201  0.    %s          %s \n',SecondaryPressure,SecondaryTemp);                    %****************************************************
        fprintf(fid,'\n');
        fprintf(fid,'*================================================================\n');

        %% ********************* HEAT STRUCTURES - HEATER TANK

        %heater in heater tank (bottom part of the
        %heater tank)
        fprintf(fid,'* heat structure data - HEATER TANK\n');
        fprintf(fid,'*----------------------------------------------------------------\n');
        fprintf(fid,'* heat structure 110 - HEATER WALL HEATED\n');
        fprintf(fid,'*         no.HS  no.m.p  geo  s.s.flag  left\n');
        fprintf(fid,'11101000  %s      5       2    0         %s \n', amount_of_heater_full_parts, radius_heater_inner);
%         fprintf(fid,'11101000  1      5       2    0         %s \n', radius_heater_inner);
        fprintf(fid,'*         mesh flag  format\n');
        fprintf(fid,'11101100  0          1\n');
        fprintf(fid,'*         intvl   right.cord.\n');
        fprintf(fid,'11101101  4       %s \n', radius_heater_outer);
        fprintf(fid,'*         comp    intvl\n');
        fprintf(fid,'11101201  001     4\n');
        fprintf(fid,'*         source  intvl\n');
        fprintf(fid,'11101301  0.0     4\n');
        fprintf(fid,'*         temp    no.m.p\n');
        fprintf(fid,'11101401  %s     5\n',Theater);       %Theater                    %****************************************************
        fprintf(fid,'*         left.vol   incr.  b.c  Surfcode  Surffactor      HS.no.\n');
        fprintf(fid,'11101501  110010000  10000  101  1       %s        %s \n', unit_vertical_height, amount_of_heater_full_parts);
        fprintf(fid,'*         right.vol  incr.  b.c  Surfcode  Surffactor      HS.no.\n');
        fprintf(fid,'11101601  0          0      2555 1       %s        %s \n', unit_vertical_height, amount_of_heater_full_parts);
        fprintf(fid,'*         s.type     mult   dir.left     dir.right  HS.no.\n');
        fprintf(fid,'11101701  0          1.     1.           1.         %s \n', amount_of_heater_full_parts);
        fprintf(fid,'*         dhy                                HS.no.\n');
        fprintf(fid,'11101801  0.0  20.  20.  0.  0.  0.  0.  1.  %s \n', amount_of_heater_full_parts);
        fprintf(fid,'*         dhy                                HS.no.\n');
        fprintf(fid,'11101901  0.0  20.  20.  0.  0.  0.  0.  1.  %s \n', amount_of_heater_full_parts);
        fprintf(fid,'*----------------------------------------------------------------\n');
        fprintf(fid,'\n'); 

        %insulation in heater tank (top part of the
        %heater tank) 
        fprintf(fid,'* heat structure 112 - HEATER WALL INSULATED\n');
        fprintf(fid,'*         no.HS  no.m.p  geo  s.s.flag  left\n');
        fprintf(fid,'11121000  %s      5       2    0         %s \n', amount_of_heater_empty_parts, radius_heater_inner);
        fprintf(fid,'*         mesh flag  format\n');
        fprintf(fid,'11121100  0          1\n');
        fprintf(fid,'*         intvl   right.cord.\n');
        fprintf(fid,'11121101  4       %s \n', radius_heater_outer);
        fprintf(fid,'*         comp    intvl\n');
        fprintf(fid,'11121201  001     4\n');
        fprintf(fid,'*         source  intvl\n');
        fprintf(fid,'11121301  0.0     4\n');
        fprintf(fid,'*         temp    no.m.p\n');
        fprintf(fid,'11121401  %s     5\n',Theater);   %Theater                        %****************************************************
        fprintf(fid,'*         left.vol   incr.  b.c  Surfcode  Surffactor      HS.no.\n');
        fprintf(fid,'11121501  112010000  10000  101  1       %s        %s \n', unit_vertical_height, amount_of_heater_empty_parts);
        fprintf(fid,'*         right.vol  incr.  b.c  Surfcode  Surffactor      HS.no.\n');
        fprintf(fid,'11121601  0          0      0    1       %s        %s \n', unit_vertical_height, amount_of_heater_empty_parts);
        fprintf(fid,'*         s.type     mult   dir.left     dir.right  HS.no.\n');
        fprintf(fid,'11121701  0          1.     1.           1.         %s \n', amount_of_heater_empty_parts);
        fprintf(fid,'*         dhy                                HS.no.\n');
        fprintf(fid,'11121801  0.0  20.  20.  0.  0.  0.  0.  1.  %s \n', amount_of_heater_empty_parts);
        fprintf(fid,'*         dhy                                HS.no.\n');
        fprintf(fid,'11121901  0.0  20.  20.  0.  0.  0.  0.  1.  %s \n', amount_of_heater_empty_parts);
        fprintf(fid,'*----------------------------------------------------------------\n');
        fprintf(fid,'\n'); 

        %% ********************** CONDENSER - ADIABATIC PART
        fprintf(fid,'* heat structure data - wall shielding primary flow from environment\n');
        fprintf(fid,'*----------------------------------------------------------------\n');
        fprintf(fid,'* heat structure 120 - ADIABATIC WALL\n');
        fprintf(fid,'*         no.HS  no.m.p  geo  s.s.flag  left\n');
        fprintf(fid,'11201000  %s     5       2    0         %s \n', amount_of_adiabatic_parts, radius_inner);
        fprintf(fid,'*         mesh flag  format\n');
        fprintf(fid,'11201100  0          1\n');
        fprintf(fid,'*         intvl   right.cord.\n');
        fprintf(fid,'11201101  4       %s \n', radius_outer);
        fprintf(fid,'*         comp    intvl\n');
        fprintf(fid,'11201201  001     4\n');
        fprintf(fid,'*         source  intvl\n');
        fprintf(fid,'11201301  0.0     4\n'); 
        fprintf(fid,'*         temp    no.m.p\n');
        fprintf(fid,'11201401  %s     5\n',Theater);                           %****************************************************
        fprintf(fid,'*         left.vol   incr.  b.c  Surfcode  Surffactor      HS.no.\n');
        if nodalizationType  % if there is not an annulus
            fprintf(fid,'11201501  120010000  10000  101  1       %s         %s \n', unit_vertical_height, amount_of_adiabatic_parts);
        else
            fprintf(fid,'11201501  121010000  10000  101  1       %s         %s \n', unit_vertical_height, amount_of_adiabatic_parts);
        end
        fprintf(fid,'*         right.vol  incr.  b.c  Surfcode  Surffactor      HS.no.\n');
        fprintf(fid,'11201601  0          0      0    1       %s         %s \n', unit_vertical_height, amount_of_adiabatic_parts);
        fprintf(fid,'*         s.type     mult   dir.left     dir.right  HS.no.\n');
        fprintf(fid,'11201701  0          1.     1.           1.         %s \n', amount_of_adiabatic_parts);
        fprintf(fid,'*         dhy\n');
        fprintf(fid,'11201801  %s  20.  20.  0.  0.  0.  0.  1.  %s \n',heat_str_hydr_D_inner, amount_of_adiabatic_parts);
        fprintf(fid,'*         dhy\n');
        fprintf(fid,'11201901  %s  20.  20.  0.  0.  0.  0.  1.  %s \n',heat_str_hydr_D_outer, amount_of_adiabatic_parts);
        fprintf(fid,'*----------------------------------------------------------------\n');
        fprintf(fid,'\n');                            

        %********************** CONDENSER - HEAT EXCHANGER
        fprintf(fid,'* heat structure data - HEAT EXCHANGER\n');
        fprintf(fid,'*----------------------------------------------------------------\n');
        fprintf(fid,'* heat structure 121 - HEAT EXCHANGER\n');
        fprintf(fid,'*         no.HS  no.m.p  geo  s.s.flag  left\n');
        fprintf(fid,'11211000  %s     5       2    0         %s \n', amount_of_heatexchange_parts, radius_inner);
        fprintf(fid,'*         mesh flag  format\n');
        fprintf(fid,'11211100  0          1\n');
        fprintf(fid,'*         intvl   right.cord.\n');
        fprintf(fid,'11211101  4       %s \n', radius_outer);
        fprintf(fid,'*         comp    intvl\n');
        fprintf(fid,'11211201  001     4\n');
        fprintf(fid,'*         source  intvl\n');
        fprintf(fid,'11211301  0.0     4\n'); 
        fprintf(fid,'*         temp    intvl\n');
        fprintf(fid,'11211401  %s     5\n',Theater);                           %****************************************************
        fprintf(fid,'*         left.vol   incr.  b.c  Surfcode  Surffactor      HS.no.\n');
        fprintf(fid,'11211501  %s  10000  101  1       %s         %s \n', condenser_starting, unit_vertical_height, amount_of_heatexchange_parts);
        fprintf(fid,'*         right.vol  incr.  b.c  Surfcode  Surffactor      HS.no.\n');
        fprintf(fid,'11211601  155010000  10000  101  1       %s         %s \n', unit_vertical_height, amount_of_heatexchange_parts);
        fprintf(fid,'*         s.type     mult   dir.left     dir.right  HS.no.\n');
        fprintf(fid,'11211701  0          1.     1.           1.         %s \n', amount_of_heatexchange_parts);
        fprintf(fid,'*         dhy\n');
        fprintf(fid,'11211801  %s  10.  10.  0.  0.  0.  0.  1.  %s \n', heat_str_hydr_D_inner,amount_of_heatexchange_parts);
        fprintf(fid,'*         dhy\n');
        fprintf(fid,'11211901  %s  10.  10.  0.  0.  0.  0.  1.  %s \n', heat_str_hydr_D_outer,amount_of_heatexchange_parts);
        fprintf(fid,'*----------------------------------------------------------------\n');
        fprintf(fid,'\n');


        %% ********************* OUTER WALL OF COOLING CHANNEL
        fprintf(fid,'* heat structure data - OUTER WALL (insulation)\n');
        fprintf(fid,'*----------------------------------------------------------------\n');
        fprintf(fid,'* heat structure 150 - OUTER WALL\n');
        fprintf(fid,'*         no.HS  no.m.p  geo  s.s.flag  left\n');
        fprintf(fid,'11501000  %s     5       2    0         %s \n', amount_of_heatexchange_parts, radius_jacket_inner);
        fprintf(fid,'*         mesh flag  format\n');
        fprintf(fid,'11501100  0          1\n');
        fprintf(fid,'*         intvl   right.cord.\n');
        fprintf(fid,'11501101  4       %s \n', radius_jacket_outer);
        fprintf(fid,'*         comp    intvl\n');
        fprintf(fid,'11501201  001     4\n');
        fprintf(fid,'*         source  intvl\n');
        fprintf(fid,'11501301  0.0     4\n');
        fprintf(fid,'*         temp    intvl\n');
        fprintf(fid,'11501401  %s     5\n',SecondaryTemp);                                    %****************************************************
        fprintf(fid,'*         left.vol   incr.  b.c  Surfcode  Surffactor      HS.no.\n');
        fprintf(fid,'11501501  155010000  10000  101  1       %s         %s \n', unit_vertical_height, amount_of_heatexchange_parts);
        fprintf(fid,'*         right.vol  incr.  b.c  Surfcode  Surffactor      HS.no.\n');
        fprintf(fid,'11501601  0          0      0    1       %s         %s \n', unit_vertical_height, amount_of_heatexchange_parts);
        fprintf(fid,'*         s.type     mult   dir.left     dir.right  HS.no.\n');
        fprintf(fid,'11501701  0          1.     1.           1.         %s \n', amount_of_heatexchange_parts);
        fprintf(fid,'*         dhy\n');
        fprintf(fid,'11501801  0.0 20.  20.  0.  0.  0.  0.  1.  %s \n', amount_of_heatexchange_parts);
        fprintf(fid,'*         dhy\n');
        fprintf(fid,'11501901  0.0 20.  20.  0.  0.  0.  0.  1.  %s \n', amount_of_heatexchange_parts);
        fprintf(fid,'=================================================================\n');
        fprintf(fid,'\n'); 

        %% PROPERTY TABLES
        fprintf(fid,'* heat structure thermal property data\n');
        fprintf(fid,'*----------------------------------------------------------------\n');

        fprintf(fid,'** stainless steel \n');
        fprintf(fid,'20100100  tbl/fctn  1  1 \n');
        fprintf(fid,'*therm. con. vs temp.     \n');
%         fprintf(fid,'20100101  265.0  7.58 \n');
%         fprintf(fid,'20100102  295.0  7.58 \n');
%         fprintf(fid,'20100103  550.0  13.43 \n');
%         fprintf(fid,'20100104  700.0  16.87 \n');
%         fprintf(fid,'20100105  873.0  20.85 \n');
%         fprintf(fid,'20100106  1173.0 27.73 \n');
%         fprintf(fid,'20100107  1671.0 29.16 \n');
%         fprintf(fid,'20100108  1727.0 30.0 \n');
%         fprintf(fid,'20100109  4000.0 30.0 \n');
        %http://www.kayelaby.npl.co.uk/general_physics/2_3/2_3_7.html
        fprintf(fid,'20100101  173.0  11.5 \n');
        fprintf(fid,'20100102  273.0  14.5 \n');
        fprintf(fid,'20100103  373.0  16.5 \n');
        fprintf(fid,'20100104  573.0  20.0 \n');
        fprintf(fid,'20100105  973.0  25.5 \n');
        fprintf(fid,'20100106  1273.0 29.5 \n');

        fprintf(fid,'*vol.ht.cap. vs temp. \n');
        fprintf(fid,'20100151  263.0  4.000e6 \n');
        fprintf(fid,'20100152  293.0  4.000e6 \n');
        fprintf(fid,'20100153  373.0  4.008e6 \n');
        fprintf(fid,'20100154  473.0  4.080e6 \n');
        fprintf(fid,'20100155  573.0  4.152e6 \n');
        fprintf(fid,'20100156  673.0  4.224e6 \n');
        fprintf(fid,'20100157  773.0  4.296e6 \n');
        fprintf(fid,'20100158  873.0  4.368e6 \n');
        fprintf(fid,'20100159  1273.0  4.440e6 \n');

        fprintf(fid,'*----------------------------------------------------------------\n');

        %% ***************** DESCRIPTION OF DELIVERED HEAT FLUX
        fprintf(fid,'* HEAT SOURCE TABLE\n');
        fprintf(fid,'*        tableType \n');
        fprintf(fid,'20255500 htrnrate \n');
        fprintf(fid,'*        time heat flux \n');
        fprintf(fid,'20255501 0.   0.  \n');                                  % Initial HEAT FLUX
        fprintf(fid,'20255502 %s.   0.  \n',action_start);   % HERE DEFINE HEAT FLUX
        fprintf(fid,'20255503 %s.   -%s  \n',action_start_end,Heat_flux_strng);   % HERE DEFINE HEAT FLUX

        fprintf(fid,'*----------------------------------------------------------------\n');
        fprintf(fid,'. end of input\n');


        fclose(fid); %close the file

        
    end
 
          disp('Input decks generated succesfully!')

end
