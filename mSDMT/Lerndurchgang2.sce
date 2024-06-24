scenario = "Lerndurchgang";

active_buttons = 3;
button_codes = 1,2,3;
default_formatted_text = true;
stimulus_properties = digit, number, symbol_nr, number, right_or_wrong, number, block, number;
event_code_delimiter = ";";
default_text_color = 0,0,0;
default_background_color = 255,255,255;

default_font_size = 60;


begin;

array{
		bitmap {filename = "C:\\Users\\neuro-forschung\\Desktop\\MS-tDCS\\Stimuli\\Set_1\\Symbol_001.jpeg"; width = 180; height=180;}target_1;
		bitmap {filename = "C:\\Users\\neuro-forschung\\Desktop\\MS-tDCS\\Stimuli\\Set_2\\Symbol_002.jpeg"; width = 180; height=180;}target_2;
		bitmap {filename = "C:\\Users\\neuro-forschung\\Desktop\\MS-tDCS\\Stimuli\\Set_3\\Symbol_003.jpeg"; width = 180; height=180;}target_3;
		bitmap {filename = "C:\\Users\\neuro-forschung\\Desktop\\MS-tDCS\\Stimuli\\Set_4\\Symbol_004.jpeg"; width = 180; height=180;}target_4;
		bitmap {filename = "C:\\Users\\neuro-forschung\\Desktop\\MS-tDCS\\Stimuli\\Set_5\\Symbol_005.jpeg"; width = 180; height=180;}target_5;
		bitmap {filename = "C:\\Users\\neuro-forschung\\Desktop\\MS-tDCS\\Stimuli\\Set_6\\Symbol_006.jpeg"; width = 180; height=180;}target_6;
		bitmap {filename = "C:\\Users\\neuro-forschung\\Desktop\\MS-tDCS\\Stimuli\\Set_7\\Symbol_007.jpeg"; width = 180; height=180;}target_7;
		bitmap {filename = "C:\\Users\\neuro-forschung\\Desktop\\MS-tDCS\\Stimuli\\Set_8\\Symbol_008.jpeg"; width = 180; height=180;}target_8;
		bitmap {filename = "C:\\Users\\neuro-forschung\\Desktop\\MS-tDCS\\Stimuli\\Set_9\\Symbol_009.jpeg"; width = 180; height=180;}target_9;
	} targets;

trial{
	#trial_type = first_response;
	trial_type = specific_response;
	terminator_button =1,2;
	trial_duration = 90000;
	
	stimulus_event {
		picture{
			bitmap target_1; x=0; y=-100;
			
			box {height = 240; width = 10; color = 0,0,0;};
			x = -540; y = 250;
			box {height = 240; width = 10; color = 0,0,0;};
			x = -420; y = 250;
			box {height = 240; width = 10; color = 0,0,0;};
			x = -300; y = 250;
			box {height = 240; width = 10; color = 0,0,0;};
			x = -180; y = 250;
			box {height = 240; width = 10; color = 0,0,0;};
			x = -60; y = 250;
			box {height = 240; width = 10; color = 0,0,0;};
			x = 60; y = 250;
			box {height = 240; width = 10; color = 0,0,0;};
			x = 180; y = 250;
			box {height = 240; width = 10; color = 0,0,0;};
			x = 300; y = 250;
			box {height = 240; width = 10; color = 0,0,0;};
			x = 420; y = 250;
			box {height = 240; width = 10; color = 0,0,0;};
			x = 540; y = 250;
			
			box {height=10; width = 1090; color = 0,0,0;};
			x = 0; y = 370;
			box {height=10; width = 1090; color = 0,0,0;};
			x = 0; y = 250;
			box {height=10; width = 1090; color = 0,0,0;};
			x = 0; y = 130;
			
			box {height=400; width = 10; color = 0,0,0;};
			x = 100; y = -200;
			box {height=400; width = 10; color = 0,0,0;};
			x = -100; y=-200;
			box {height=10;width=210; color=0,0,0;};
			x = 0; y = -0;
			box {height=10;width=210; color=0,0,0;};
			x = 0; y = -200;
			box {height=10;width=210; color=0,0,0;};
			x = 0; y = -400;
			
			bitmap {filename = "C:\\Users\\neuro-forschung\\Desktop\\MS-tDCS\\Stimuli\\Set_1\\Symbol_001.jpeg"; width=100; height=100;} graphic_1;
			x = -480; y = 310;
			bitmap {filename = "C:\\Users\\neuro-forschung\\Desktop\\MS-tDCS\\Stimuli\\Set_1\\Symbol_002.jpeg"; width=100; height=100;} graphic_2;
			x = -360; y = 310;
			bitmap {filename = "C:\\Users\\neuro-forschung\\Desktop\\MS-tDCS\\Stimuli\\Set_1\\Symbol_003.jpeg"; width=100; height=100;} graphic_3;
			x = -240; y = 310;
			bitmap {filename = "C:\\Users\\neuro-forschung\\Desktop\\MS-tDCS\\Stimuli\\Set_1\\Symbol_004.jpeg"; width=100; height=100;} graphic_4;
			x = -120; y = 310;
			bitmap {filename = "C:\\Users\\neuro-forschung\\Desktop\\MS-tDCS\\Stimuli\\Set_1\\Symbol_005.jpeg"; width=100; height=100;} graphic_5;
			x = 0; y = 310;
			bitmap {filename = "C:\\Users\\neuro-forschung\\Desktop\\MS-tDCS\\Stimuli\\Set_1\\Symbol_006.jpeg"; width=100; height=100;} graphic_6;
			x = 120; y = 310;
			bitmap {filename = "C:\\Users\\neuro-forschung\\Desktop\\MS-tDCS\\Stimuli\\Set_1\\Symbol_007.jpeg"; width=100; height=100;} graphic_7;
			x = 240; y = 310;
			bitmap {filename = "C:\\Users\\neuro-forschung\\Desktop\\MS-tDCS\\Stimuli\\Set_1\\Symbol_008.jpeg"; width=100; height=100;} graphic_8;
			x = 360; y = 310;
			bitmap {filename = "C:\\Users\\neuro-forschung\\Desktop\\MS-tDCS\\Stimuli\\Set_1\\Symbol_009.jpeg"; width=100; height=100;} graphic_9;
			x = 480; y = 310;
			
			text {caption = "1";}legend_1;
			x = -480; y = 190;
			text {caption = "2";}legend_2;
			x = -360; y = 190;
			text {caption = "3";}legend_3;
			x = -240; y = 190;
			text {caption = "4";}legend_4;
			x = -120; y = 190;
			text {caption = "5";}legend_5;
			x = 0; y = 190;
			text {caption = "6";}legend_6;
			x = 120; y = 190;
			text {caption = "7";}legend_7;
			x = 240; y = 190;
			text {caption = "8";}legend_8;
			x = 360; y = 190;
			text {caption = "9";}legend_9;
			x = 480; y = 190;
			
			text {caption = "1";}target;
			x = 0; y = -300;
		}legend;
		time = 0;
		#target_button = 1;
	}right_or_wrong;
}pres;

trial{
	trial_type = first_response;
	trial_duration = forever;
	
	stimulus_event {
		picture{
			text  {font_size=30; caption = "Sehr gut, Sie haben Block X von 9 geschafft. \n Dabei haben Sie ...% der Paarungen richtig bewertet. Ihre durchschnittliche Reaktionszeit betrug ... Sekunden. \n\n\n Die Aufgabe wird durch Druck von ... fortgeführt.";}intertrial_text;
			x = 0; y = 0;
		}intertrial_picture;
		time = 0;
	};
	stimulus_event{
		picture intertrial_picture;
		time = 200;
	};
}intertrial;

trial {
	trial_duration = 2000;
	
	stimulus_event{
		picture intertrial_picture;
		time = 0;
	};
}intertrial2;

trial{
	trial_type = first_response;
	trial_duration = forever;
	
	stimulus_event{
		picture{
			text {font_size = 30; caption = "In der folgenden Aufgabe wird Ihnen am oberen Bildschirmrand eine Legende angezeigt.\n Diese beinhaltet Zahlen von 1 bis 9. Unter jeder dieser Zahlen ist ein Symbol zu sehen.\nDie Zahlen und die Symbole bilden Paare. \nAm unteren Bildschirmrand wird ebenfalls eine Zahl und ein Symbol zu sehen sein.\n Ihre Aufgabe wird es sein, zu beurteilen, ob die Paarung von Zahl und Symbol der in der Legende entspricht.\n Dazu drücken sie die Taste M, wenn die Paarung genau wie in der Legende ist,\n und die Taste Y, wenn die Paarung ungleich ist. Am besten lassen Sie die Zeigefinger einfach auf den Tasten liegen. \nVersuchen Sie so schnell wie möglich zu antworten ohne einen Fehler zu machen. Die Aufgabe beinhaltet 9 Blocks. \nNach jedem Block haben Sie eine kleine Pause und die Symbole, die sie beurteilen sollen, ändern sich.\n\n\n Der Versuch beginnt mit Druck der Leertaste.";}inst_text;
			x=0;y=0;
		};
		time=0;
	};
}instruction;

picture {
   text { caption = "Versionsauswahl (ver1 oder ver2):"; };
   x = 0; y = 100;
   text { caption = " "; } text1;
   x = 0; y = 0;
} pic1;

begin_pcl;

int nr_sets = 1;
int nr_digits = 9;
int nr_dist_sets = 8;

array <int> stimuli_matching [nr_digits*2][3][nr_dist_sets];
loop int dist_count=1 until dist_count > nr_dist_sets
begin
	loop int line_2_18=1 until line_2_18 > nr_digits*2
	begin
		if line_2_18 <= 9 then
			stimuli_matching [line_2_18][1][dist_count] = line_2_18;
			stimuli_matching [line_2_18][2][dist_count] = line_2_18;
			stimuli_matching [line_2_18][3][dist_count] = 1;
		else
			if line_2_18-9+dist_count > 9 then
				stimuli_matching [line_2_18][1][dist_count] = line_2_18-9;
				stimuli_matching [line_2_18][2][dist_count] = line_2_18+dist_count-18;
				stimuli_matching [line_2_18][3][dist_count] = 0;
			else
				stimuli_matching [line_2_18][1][dist_count] = line_2_18-9;
				stimuli_matching [line_2_18][2][dist_count] = line_2_18+dist_count-9;
				stimuli_matching [line_2_18][3][dist_count] = 0;
			end;
		end;
		line_2_18=line_2_18+1;
	end;
	dist_count=dist_count+1;
end;

sub bool ok_line (array <int,1> & input, int set) 
begin
	loop int i = 2 until i > input.count ()
	begin
		if stimuli_matching [input[i]][1][set] == stimuli_matching [input [i-1]][1][set] || stimuli_matching[input[i]][2][set] == stimuli_matching[input[i-1]][2][set] then
			return false
		end;
		i=i+1
	end;
	return true
end;
	
sub array <int,1> proof_line (array <int,1> & input, int set)
begin
	loop input.shuffle() until ok_line (input, set) == true
	begin
		input.shuffle();
	end;
	return input;
end;

sub bool order_ok (array <int,3> & input, int set)
begin
	loop int i = 2 until i > input.count()
	begin
		if input [i][2][set] == input [i-1][2][set] || input [i][3][set] == input [i-1][3][set] then
			return false;
		i=i+1;
		end;
	end;
	return true;
end;

array <int> pres_order [144][4][nr_sets];
array <int> dist_set [nr_dist_sets] = {1, 2, 3, 4, 5, 6, 7, 8};
array <int> sets [nr_sets] = {1, 2, 3, 4, 5, 6, 7, 8, 9};

sets.shuffle();

loop int set_count = 1 until set_count > nr_sets
begin
	int line_count = 1;
	dist_set.shuffle();
	loop int dist_count = 1 until dist_count > nr_dist_sets
	begin
		array <int> random_line [nr_digits*2];
		
		loop int line_2_18 =1 until line_2_18 > nr_digits*2
		begin
			random_line [line_2_18] = line_2_18;
			line_2_18=line_2_18+1;
		end;
			
		random_line = proof_line (random_line, dist_set[dist_count]);
			
		loop int line_2_18 = 1 until line_2_18 > nr_digits*2
		begin
			pres_order [line_count][1][set_count] = sets[set_count];
			pres_order [line_count][2][set_count] = stimuli_matching[random_line[line_2_18]][1][dist_set[dist_count]];
			pres_order [line_count][3][set_count] = stimuli_matching[random_line[line_2_18]][2][dist_set[dist_count]];
			pres_order [line_count][4][set_count] = stimuli_matching[random_line[line_2_18]][3][dist_set[dist_count]];
			line_2_18=line_2_18+1;
			line_count=line_count+1;
		end;
		dist_count=dist_count+1;
	end;
	set_count=set_count+1;
end;

loop int set_count = 1 until set_count > nr_sets
begin
	loop int line_count = 2 until line_count > nr_digits*2*nr_dist_sets
	begin
		if pres_order [line_count][2][set_count] == pres_order [line_count-1][2][set_count]||pres_order[line_count][3][set_count]==pres_order[line_count-1][3][set_count]then
			int switch_1_1 = pres_order [line_count][1][set_count];
			int switch_1_2 = pres_order [line_count][2][set_count];
			int switch_1_3 = pres_order [line_count][3][set_count];
			int switch_1_4 = pres_order [line_count][4][set_count];
			
			int switch_2_1 = pres_order [line_count+1][1][set_count];
			int switch_2_2 = pres_order [line_count+1][2][set_count];
			int switch_2_3 = pres_order [line_count+1][3][set_count];
			int switch_2_4 = pres_order [line_count+1][4][set_count];
			
			pres_order [line_count][1][set_count]=switch_2_1;
			pres_order [line_count][2][set_count]=switch_2_2;
			pres_order [line_count][3][set_count]=switch_2_3;
			pres_order [line_count][4][set_count]=switch_2_4;
			
			pres_order [line_count+1][1][set_count]=switch_1_1;
			pres_order [line_count+1][2][set_count]=switch_1_2;
			pres_order [line_count+1][3][set_count]=switch_1_3;
			pres_order [line_count+1][4][set_count]=switch_1_4;
		end;	
		line_count = line_count+1
	end;
	set_count = set_count+1;
end;

system_keyboard.set_delimiter('\n');
system_keyboard.set_max_length(20);
system_keyboard.set_time_out(100000);
string input = system_keyboard.get_input(pic1, text1);
if (system_keyboard.last_input_type() != keyboard_delimiter) then
   term.print("You must press return!")
end;

if input == "ver1" then
		inst_text.set_caption ("In der folgenden Aufgabe wird Ihnen am oberen Bildschirmrand eine Legende angezeigt.\n Diese beinhaltet Zahlen von 1 bis 9. Unter jeder dieser Zahlen ist ein Symbol zu sehen.\nDie Zahlen und die Symbole bilden Paare. \nAm unteren Bildschirmrand wird ebenfalls eine Zahl und ein Symbol zu sehen sein.\n Ihre Aufgabe wird es sein, zu beurteilen, ob die Paarung von Zahl und Symbol der in der Legende entspricht.\n Dazu drücken Sie die Taste M, wenn die Paarung genau wie in der Legende ist,\n und die Taste Y, wenn die Paarung ungleich ist. Am besten lassen Sie die Zeigefinger einfach auf den Tasten liegen. \nVersuchen Sie so schnell wie möglich zu antworten ohne einen Fehler zu machen. Die Aufgabe beinhaltet 9 Blocks. \nNach jedem Block haben Sie eine kleine Pause und die Symbole, die Sie beurteilen sollen, ändern sich.\n\n\n Der Versuch beginnt mit Druck der Leertaste.");
elseif input == "ver2" then
		inst_text.set_caption ("In der folgenden Aufgabe wird Ihnen am oberen Bildschirmrand eine Legende angezeigt.\n Diese beinhaltet Zahlen von 1 bis 9. Unter jeder dieser Zahlen ist ein Symbol zu sehen.\nDie Zahlen und die Symbole bilden Paare. \nAm unteren Bildschirmrand wird ebenfalls eine Zahl und ein Symbol zu sehen sein.\n Ihre Aufgabe wird es sein, zu beurteilen, ob die Paarung von Zahl und Symbol der in der Legende entspricht.\n Dazu drücken Sie die Taste Y, wenn die Paarung genau wie in der Legende ist,\n und die Taste M, wenn die Paarung ungleich ist. Am besten lassen Sie die Zeigefinger einfach auf den Tasten liegen. \nVersuchen Sie so schnell wie möglich zu antworten ohne einen Fehler zu machen. Die Aufgabe beinhaltet 9 Blocks. \nNach jedem Block haben Sie eine kleine Pause und die Symbole, die Sie beurteilen sollen, ändern sich.\n\n\n Der Versuch beginnt mit Druck der Leertaste.");
end;

inst_text.redraw();

instruction.present();

int block_duration = 450000;
double percentage_right = 0.0;

string path = "C:\\Users\\neuro-forschung\\Desktop\\MS-tDCS\\Stimuli\\";
	
loop int q = 1 until q > 1
begin
	
	string set_name = string(pres_order[1][1][q]);
	
	graphic_1.set_filename (path+"Set_1\\Symbol_003.jpeg");
	graphic_1.load();
	graphic_2.set_filename (path+"Set_2\\Symbol_004.jpeg");
	graphic_2.load();
	graphic_3.set_filename (path+"Set_3\\Symbol_005.jpeg");
	graphic_3.load();
	graphic_4.set_filename (path+"Set_4\\Symbol_006.jpeg");
	graphic_4.load();
	graphic_5.set_filename (path+"Set_5\\Symbol_007.jpeg");
	graphic_5.load();
	graphic_6.set_filename (path+"Set_6\\Symbol_008.jpeg");
	graphic_6.load();
	graphic_7.set_filename (path+"Set_7\\Symbol_009.jpeg");
	graphic_7.load();
	graphic_8.set_filename (path+"Set_8\\Symbol_001.jpeg");
	graphic_8.load();
	graphic_9.set_filename (path+"Set_9\\Symbol_002.jpeg");
	graphic_9.load();
	
	target_1.set_filename (path+"Set_1\\Symbol_003.jpeg");
	target_1.load();
	target_2.set_filename (path+"Set_2\\Symbol_004.jpeg");
	target_2.load();
	target_3.set_filename (path+"Set_3\\Symbol_005.jpeg");
	target_3.load();
	target_4.set_filename (path+"Set_4\\Symbol_006.jpeg");
	target_4.load();
	target_5.set_filename (path+"Set_5\\Symbol_007.jpeg");
	target_5.load();
	target_6.set_filename (path+"Set_6\\Symbol_008.jpeg");
	target_6.load();
	target_7.set_filename (path+"Set_7\\Symbol_009.jpeg");
	target_7.load();
	target_8.set_filename (path+"Set_8\\Symbol_001.jpeg");
	target_8.load();
	target_9.set_filename (path+"Set_9\\Symbol_002.jpeg");
	target_9.load();
	
	array <int> symbol_nr [nr_digits*2*nr_dist_sets]; 
	loop int i = 1 until i > nr_digits*2*nr_dist_sets
	begin
		symbol_nr[i] = pres_order [i][3][q];
		i=i+1;
	end;
	
	array <bitmap> symbols[0];
	loop int i = 1 until i > nr_digits*2*nr_dist_sets
	begin
		symbols.add(targets[symbol_nr[i]]);
		i=i+1;
	end;
	
	double hit_count = 0.0;
	double miss_incorrect_count = 0.0;
	
	int start_time = clock.time();
	int now = clock.time();
	
	loop int i = 1 until i > 10
	begin
		if pres_order [i][4][q] == 1 && input == "ver1" then
			right_or_wrong.set_target_button(2);
		elseif pres_order [i][4][q] == 0 && input == "ver1" then
			right_or_wrong.set_target_button(1);
		elseif pres_order [i][4][q] == 1 && input == "ver2" then
			right_or_wrong.set_target_button(1);
		elseif pres_order [i][4][q] == 0 && input == "ver2" then
			right_or_wrong.set_target_button(2)
		end;
		int remaining_block_duration = block_duration - (now - start_time);
		pres.set_duration (remaining_block_duration);
		right_or_wrong.set_event_code (string(pres_order[i][2][q])+";"+string(pres_order[i][3][q])+";"+string(pres_order[i][4][q])+";"+string(q));
		legend.set_part (1,targets[pres_order[i][3][q]]);
		target.set_caption(string(pres_order[i][2][q]));
		target.redraw();
		pres.present();
		now = clock.time();
		stimulus_data last = stimulus_manager.last_stimulus_data();
		
		if last.type() == stimulus_hit then
			hit_count = hit_count+1;
		elseif last.type() == stimulus_miss || last.type() == stimulus_incorrect then
			miss_incorrect_count = miss_incorrect_count+1;
		end;
		
		if now - start_time > block_duration then
			break;
		end;
		i=i+1;
	end;
	
	double decimal_right = 0.00;
	decimal_right = hit_count/(hit_count+miss_incorrect_count);
	percentage_right = decimal_right*100;
	
	if percentage_right>90 then
		intertrial_text.set_caption ("Sehr gut, Sie haben den Lerndurchgang geschafft. \n Dabei haben Sie " + string(round(percentage_right,0)) + "% der Paarungen richtig bewertet. \n\n\n Die Aufgabe wird durch Druck der Leertaste wird der Lerndurchgang beendet.");
	else
		intertrial_text.set_caption ("Im aktuellen Lerndurchgang haben Sie " + string(round(percentage_right,0)) + "% der Paarungen richtig bewertet. \nDie Aufgabe startet, wenn Sie 75% der Paarungen richtig bewerten.\n\n\n Der Lerndurchgang wird durch Druck der Leertaste wiederholt.");
	end;

	intertrial_text.redraw();
	
	intertrial2.present();
	intertrial.present();
	
end;

