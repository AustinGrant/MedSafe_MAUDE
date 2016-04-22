# Downloads all the MAUDE (including FOIDEV and MDRFOI) files from the FDA Website
# Searches for the records related to a specific device
# Merges the FIODEV and MDRFOI files into one Excel sheet
# Goes through MDR keys and opens up the MDR reports on FDA website to grab and add
# Patient outcome, Event Description and Narrative, and Number of Devices to the table

import urllib, urllib2, cookielib
import BeautifulSoup
import csv
import re
from nltk.probability import ConditionalFreqDist
from nltk.tokenize import word_tokenize
from nltk.stem.wordnet import WordNetLemmatizer
from nltk.corpus import stopwords
import string
from zipfile import ZipFile
import os
import xlrd, xlwt
import time
from datetime import date
from dateutil import parser
cj = cookielib.CookieJar()
opener = urllib2.build_opener(urllib2.HTTPCookieProcessor(cj))

#### Extract the fields from each record
def FieldExtract(line, field_numbers):
    fields = line.split('|');
    extracted = [];
    for f in field_numbers:
        extracted.append(fields[f-1].strip());
    #print extracted
    return extracted

###### Download the data files from MAUDE database and save it 
def MAUDE_Download(FOIDEV_files, MDRFOI_files, data_dir):
    MAUDE_url = 'http://www.accessdata.fda.gov/MAUDE/ftparea/'
    os.chdir(data_dir)
    # Download All FOIDEV files   
    for filename in FOIDEV_files + MDRFOI_files:            
        # Download the Zip file
        with open(data_dir+filename+'.zip', 'wb') as zfile:
            zfile.write(urllib2.urlopen(MAUDE_url+filename+'.zip').read())
        # Extract the Zip file
        zip_data = ZipFile(data_dir+filename+'.zip', 'r').extractall(data_dir)
        # Clean up the folder by deleting the Zip file
        os.remove(data_dir+filename+'.zip');
        print filename+' downloaded.'				

#### Extract the FOIDEV records and append them to the file
def FOIDEVExtract(FOIDEV_files, FOIDEV_Field_Numbers, device_name, device_keywords, data_dir):
    foidev_count = 0;
    os.chdir(data_dir)
    
    print 'Starting to extract da Vinci related records..'
    # Extract those related to the device
    for filename in FOIDEV_files:
        # If the first file, first get the titles
        if (filename == FOIDEV_files[0]):
            with open(data_dir+filename+'.txt', "rb") as foidev_file:
                title = foidev_file.next();

                # Create the Hash Table of FOIDEV records
                FOIDEV_Titles = FieldExtract(title, FOIDEV_Field_Numbers);
                device_MDR_Hash = {'MDR_Key':FOIDEV_Titles};

                # Write the titles
                with open(device_name+'_FOIDEV.txt', "w") as myfile:
                    myfile.write('|'.join(FOIDEV_Titles)+'\n');
                myfile.close();             
            
        foidev_file.close();
            
        # Extract only those FOIDEV records related to the Device
        # => Write in the Hash Table and in 'device_FOIDEV.txt'        
        with open(data_dir+filename+'.txt', "rb") as foidev_file:
            for line in foidev_file:
                for k in device_keywords: 
                    if (line.lower().find(k) > -1):
                        MDR_Key = line.split('|')[0];
                        if (not device_MDR_Hash.has_key(MDR_Key)):
                            #print MDR_Key;                                     
                            FOIDEV_Fields = FieldExtract(line, FOIDEV_Field_Numbers);  						
                            device_MDR_Hash[MDR_Key] = FOIDEV_Fields;
                            foidev_count = foidev_count+1;
                            #print foidev_count                            
                            # Write FOIDEV Columns
                            with open(device_name+'_FOIDEV.txt', "a") as myfile:
                                myfile.write('|'.join(FOIDEV_Fields)+'\n');
                            myfile.close();                                
                            break;
        foidev_file.close();
   
    print str(foidev_count)+' FOIDEV records extracted and added to the table.' 
    return device_MDR_Hash;

#### Extract the FOIDEV records and append them to the file
def FOIDEVExtract2(FOIDEV_files, FOIDEV_Field_Numbers, device_name, device_codes, data_dir):
    foidev_count = 0;
    os.chdir(data_dir)
    
    # Extract those related to the device
    for filename in FOIDEV_files:
        # If the first file, first get the titles
        if (filename == FOIDEV_files[0]):
            with open(data_dir+filename+'.txt', "rb") as foidev_file:
                title = foidev_file.next();

                # Create the Hash Table of FOIDEV records
                FOIDEV_Titles = FieldExtract(title, FOIDEV_Field_Numbers);
                device_MDR_Hash = {'MDR_Key':FOIDEV_Titles};

                # Write the titles
                with open(device_name+'_FOIDEV.txt', "w") as myfile:
                    myfile.write('|'.join(FOIDEV_Titles)+'\n');
                myfile.close();             
            
        foidev_file.close();
            
        # Extract only those FOIDEV records related to the Device
        # => Write in the Hash Table and in 'device_FOIDEV.txt'        
        with open(data_dir+filename+'.txt', "rb") as foidev_file:
            for line in foidev_file:
                for k in device_codes:
                    if (line.find(k) > -1):
                        MDR_Key = line.split('|')[0];
                        if (not device_MDR_Hash.has_key(MDR_Key)):
                            #print MDR_Key;                                     
                            FOIDEV_Fields = FieldExtract(line, FOIDEV_Field_Numbers);  						
                            device_MDR_Hash[MDR_Key] = FOIDEV_Fields;
                            foidev_count = foidev_count+1;
                            #print foidev_count                            
                            # Write FOIDEV Columns
                            with open(device_name+'_FOIDEV.txt', "a") as myfile:
                                myfile.write('|'.join(FOIDEV_Fields)+'\n');
                            myfile.close();                                
                            break;
        foidev_file.close();
   
    print str(foidev_count)+' FOIDEV records extracted and added to the table.' 
    return device_MDR_Hash;

def Get_Other_Fields(MDR_Link):
    # Open each MDR Link
    time.sleep(0.25)
    result = urllib2.urlopen(MDR_Link);
    soup = BeautifulSoup.BeautifulSoup(result)
    
    ##### Patient Outcome, Event Description, and Manufacturer Narrative
    regex = re.compile(r'\s*[\n\r\t]')
    Patient_Outcome = 'N/A';  
    Event = '';
    Narrative = '';
    for st in soup.findAll('strong'):
        # Patient Outcome
        if (st.string.count('Patient Outcome') > 0):
            if (st.next.next != ''):
                Raw_Outcome = st.next.next;                
                Patient_Outcome = regex.sub('', Raw_Outcome).strip().encode('ascii','ignore').replace("&nbsp;", "");

        # Event Description        
        if (st.string.find('Event Description') > 0):
            if (st.findNext('p').contents != []):
                Raw_Event = st.findNext('p').contents[0];
                Event = Event + regex.sub('', Raw_Event).strip().encode('ascii','ignore') + ' ';
                
        # Manufacturer Narrative
        if (st.string.find('Manufacturer Narrative') > 0):
            if (st.findNext('p').contents != []):
                Raw_Narrative = st.findNext('p').contents[0];
                Narrative = Narrative + regex.sub('', Raw_Narrative).strip().encode('ascii','ignore') + ' ';
    # If not found any narrative or event description
    if (Event == ''):
        Event = 'N/A'
    if (Narrative == ''):             
        Narrative = 'N/A'        
        
    ##### Number of Devices 
    for st in soup.findAll('th'):
        if (len(st.contents) > 1):            
            if ((st.contents[1].string.strip().encode('ascii','ignore').count('Device Was Involved in the Event')>0) or           
	       (st.contents[1].string.strip().encode('ascii','ignore').count('DeviceS WERE Involved in the Event')>0)):                  

                Number_Devices = st.contents[0].contents[0].string.strip().encode('ascii','ignore');
                break;
    return [Patient_Outcome, Event, Narrative]
            
def MAUDE_Merge_Tables(end_year, FOIDEV_files, MDRFOI_files, FOIDEV_Field_Numbers,
                       MDRFOI_Field_Numbers, device_name, data_dir):
    os.chdir(data_dir)
    MAUDE_Keys = [];
    AllCounts = [0,0,0]
    
    # Optimized MAUDE Data Output
    newbook = xlwt.Workbook("iso-8859-2")
    newsheet = newbook.add_sheet('Maude_Data', cell_overwrite_ok = True)

    f1 = open('./'+device_name+'_MAUDE_Data_'+str(end_year)+'.csv', 'wb')
    csv_wr = csv.writer(f1, dialect='excel', delimiter=',')    

    # Extract the Titles of Fields of Interest
    # FOIDEV_Titles
    with open(data_dir+FOIDEV_files[0]+'.txt', "rb") as foidev_file:
        title = foidev_file.next();
        FOIDEV_titles = FieldExtract(title, FOIDEV_Field_Numbers);
    
    # MDRFOI_titles
    with open(data_dir+MDRFOI_files[0]+'.txt', "rb") as mdrfoi_file:
        title = mdrfoi_file.next();
        MDRFOI_titles = FieldExtract(title, MDRFOI_Field_Numbers);
    
    # Create device_MDR_Hash
    device_MDR_Hash = {'MDR_Key':FOIDEV_titles};
    with open(device_name+'_FOIDEV.txt', "r") as foidev_file:
        # Skip the title
        title = foidev_file.next();
        for line in foidev_file:
            FOIDEV_Fields = line.split('|');
            MDR_Key = FOIDEV_Fields[0].strip();
            device_MDR_Hash[MDR_Key] = FOIDEV_Fields;
            #print FOIDEV_Fields;
    print ('Number of records = '+str(len(device_MDR_Hash))+'\n')
    
    # Cross-match MDRFOI files to FOIDEV file
    curr_row = 0;
    for filename in MDRFOI_files:
        with open(data_dir+filename+'.txt', 'rb') as mdrfoi_file:
            # Skip the title
            title = mdrfoi_file.next();

            # If first time, write the titles
            if (filename == MDRFOI_files[0]):
                newsheet.write(curr_row, 0, 'MDR_Link')
                newsheet.write(curr_row, 1, 'Patient_Outcome')
                newsheet.write(curr_row, 2, 'Event')
                newsheet.write(curr_row, 3, 'Narrative')
                newsheet.write(curr_row, 4, 'Manufacture Year')
                newsheet.write(curr_row, 5, 'Event Year')
                newsheet.write(curr_row, 6, 'Report Year')
                newsheet.write(curr_row, 7, 'Time to Event')
                newsheet.write(curr_row, 8, 'Time to Report')                
                curr_col = 9;
                # Write MDRFOI Titles
                for i in range(0, len(MDRFOI_titles)):
                    newsheet.write(curr_row, curr_col+i, MDRFOI_titles[i])
                # Write FOIDEBV Titles
                for i in range(0, len(FOIDEV_titles)):
                    newsheet.write(curr_row, curr_col+len(MDRFOI_titles)+i, FOIDEV_titles[i]) 
                # Goto the next row
                curr_col = 0;
                curr_row = 1;

                csv_wr.writerow(['MDR_Link', 'MDR_Key', 'Event', 'Narrative', 'Event_Type','Patient_Outcome',
                                 'Manufacture Year','Event_Year','Report_to_Manufacture_Year','Report_to_FDA','Report_Year','Time_to_Event','Time_to_Report',
                                 'Manufacturer', 'Brand_Name','Generic_Name','Product_Code'])

            # For each file, read Each Line and Cross-Match it to FOIDEV    
            for line in mdrfoi_file:
                MDRFOI_fields = FieldExtract(line, MDRFOI_Field_Numbers);
                MDR_Key = MDRFOI_fields[0];
                Event_Type = MDRFOI_fields[MDRFOI_titles.index('EVENT_TYPE')];
                if MAUDE_Keys.count(MDR_Key) == 0:
                    MAUDE_Keys.append(MDR_Key);
                    AllCounts[0] = AllCounts[0]+1;
                    if Event_Type == 'D':
                        AllCounts[1] = AllCounts[1]+1;
                    elif Event_Type == 'IN':
                        AllCounts[2] = AllCounts[2]+1;
                    
                if (device_MDR_Hash.has_key(MDR_Key)):# or (MDR_Key == '2222833'):
                    # Get the report year
                    if (MDRFOI_fields[MDRFOI_titles.index('DATE_RECEIVED')] != ''):
                        Report_DateStr = MDRFOI_fields[MDRFOI_titles.index('DATE_RECEIVED')];
                        Report_Date = parser.parse(Report_DateStr);                        
                        Report_Year = str(Report_Date.year);                         
                    else:
                        Report_Date = 'N/A';
                        Report_Year = 'N/A';

                    # Only if the report year is before the end year
                    if (int(Report_Year) <= end_year):                       
                        # Get the rest of the fields from online records
                        MDR_Link = 'http://www.accessdata.fda.gov/scripts/cdrh/cfdocs/cfMAUDE/Detail.cfm?MDRFOI__ID='+MDR_Key;
                        print str(curr_row)+'='+MDR_Key+'\n';
                        [Patient_Outcome, Event, Narrative] = Get_Other_Fields(MDR_Link);
                        MDR_HLink = 'HYPERLINK("'+MDR_Link+'";"'+MDR_Link+'")';

                        # Correct the EVENT Type
                        Event_Type = MDRFOI_fields[MDRFOI_titles.index('EVENT_TYPE')];
                        if (Event_Type == '*') or (Event_Type == ''):
                            Event_Type = 'O'

                        # Extract all the time fields 
                        if (MDRFOI_fields[MDRFOI_titles.index('DEVICE_DATE_OF_MANUFACTURE')] != ''):
                            Manufacture_DateStr = MDRFOI_fields[MDRFOI_titles.index('DEVICE_DATE_OF_MANUFACTURE')].strip();
                            Manufacture_Date = parser.parse(Manufacture_DateStr);                        
                            Manufacture_Year = str(Manufacture_Date.year);
                        else:
                            Manufacture_Date = 'N/A';
                            Manufacture_Year = 'N/A';

                        if (MDRFOI_fields[MDRFOI_titles.index('DATE_OF_EVENT')] != ''):
                            Event_DateStr = MDRFOI_fields[MDRFOI_titles.index('DATE_OF_EVENT')];
                            Event_Date = parser.parse(Event_DateStr);                        
                            Event_Year = str(Event_Date.year);                        
                        else:
                            Event_Date = 'N/A';
                            Event_Year = 'N/A';

                        if (MDRFOI_fields[MDRFOI_titles.index('DATE_REPORT')] != ''):
                            ReportMade_DateStr = MDRFOI_fields[MDRFOI_titles.index('DATE_REPORT')];
                            ReportMade_Date = parser.parse(ReportMade_DateStr);                        
                            ReportMade_Year = str(ReportMade_Date.year);                         
                        else:
                            ReportMade_Date = 'N/A';
                            ReportMade_Year = 'N/A';

                        if (MDRFOI_fields[MDRFOI_titles.index('DATE_REPORT_TO_MANUFACTURER')] != ''):
                            ReportMan_DateStr = MDRFOI_fields[MDRFOI_titles.index('DATE_REPORT_TO_MANUFACTURER')];
                            ReportMan_Date = parser.parse(ReportMan_DateStr);                        
                            ReportMan_Year = str(ReportMan_Date.year);                         
                        else:
                            ReportMan_Date = 'N/A';
                            ReportMan_Year = 'N/A';

                        if Manufacture_Date != 'N/A' and Event_Date != 'N/A' and Event_Date > Manufacture_Date:
                            Time_to_Event = str((Event_Date - Manufacture_Date).days)
                        else:
                            Time_to_Event = 'N/A'
                        if Event_Date != 'N/A' and Report_Date != 'N/A' and Report_Date > Event_Date:
                            Time_to_Report = str((Report_Date - Event_Date).days)
                        else:
                            Time_to_Report = 'N/A'                      

                        # Write the extracted of MDRFOI Columns from online records
                        newsheet.write(curr_row, 0, xlwt.Formula(MDR_HLink))
                        newsheet.write(curr_row, 1, Patient_Outcome)
                        newsheet.write(curr_row, 2, Event)
                        newsheet.write(curr_row, 3, Narrative)
                        newsheet.write(curr_row, 4, Manufacture_Year)
                        newsheet.write(curr_row, 5, Event_Year)
                        newsheet.write(curr_row, 6, Report_Year)
                        newsheet.write(curr_row, 7, Time_to_Event)
                        newsheet.write(curr_row, 8, Time_to_Report)                        
                        curr_col = 9;
                        
                        # Write the rest of MDRFOI Columns
                        for i in range(0, len(MDRFOI_titles)):
                            if MDRFOI_titles[i].find('EVENT_TYPE') > -1:
                                newsheet.write(curr_row, curr_col+i, Event_Type)    
                            else:
                                newsheet.write(curr_row, curr_col+i, MDRFOI_fields[i])
                        # Write FOIDEV Columns
                        for i in range(0, len(FOIDEV_titles)):
                            newsheet.write(curr_row, curr_col+len(MDRFOI_titles)+i, device_MDR_Hash[MDR_Key][i])          

                        # Write selected columns to CSV file
                        Manufacturer = device_MDR_Hash[MDR_Key][4];
                        Brand_Name = device_MDR_Hash[MDR_Key][2];
                        Generic_Name = device_MDR_Hash[MDR_Key][3];
                        Product_Code = device_MDR_Hash[MDR_Key][6];
                        print Manufacturer
                        print Brand_Name
                        print Generic_Name
                        print Product_Code                    
                        csv_wr.writerow([MDR_Link, MDR_Key, Event, Narrative, Event_Type, Patient_Outcome,
                                         Manufacture_Year, Event_Year, ReportMan_Year, ReportMade_Year, Report_Year, Time_to_Event, Time_to_Report,
                                         Manufacturer, Brand_Name, Generic_Name, Product_Code])

                        # Remove the record from the hash to avoid duplicate records
                        device_MDR_Hash.pop(MDR_Key);

                        # Goto the next row
                        curr_row = curr_row + 1;
        mdrfoi_file.close();
        
    print str(curr_row)+' MDRFOI records cross-matched with FOIDEV records, and saved to the XLS file.'    
    newbook.save(data_dir+device_name+'_MAUDE_Data_'+str(end_year)+'.xls')
    return AllCounts
###### Parameters
# Fields of interest (Numbers are based on Field Numbers provided on the FDA Website)
FOIDEV_Field_Numbers = [1,2,7,8,9,19,26,27,29,30,31,34,36,44]
MDRFOI_Field_Numbers = [1,2,3,4,6,7,8,9,10,11,12,14,16,17,25,26,27,28,30,31,32,68,69,70,72,75]
# Years
start_year = 2000
current_year = 2014
end_year = 2013 
# Device of Interest
device_name = ['daVinci','pacemaker','patient_monitor']
# Device Keywords
device_keywords = [['da vinci', 'davinci', 'davency', 'davincy', 'davincy',
                    'intuitive surgical', 'intuitivesurgical'],['pacemaker']]
# Data Directory
data_dir = './';

####### Generate the FOIDEV Filenames
FOIDEV_files = [];
for years in range(start_year,current_year):               
    FOIDEV_files.append('foidev'+str(years));
FOIDEV_files = FOIDEV_files + ['foidevchange','foidev']
####### Generate the MDRFOI Filenames
MDRFOI_files = ['mdrfoithru'+str(current_year-1),'mdrfoi','mdrfoichange']

####### Download Maude Data
#MAUDE_Download(FOIDEV_files, MDRFOI_files, data_dir)

####### Extract FOIDEV files for the device of interest
FOIDEVExtract(FOIDEV_files, FOIDEV_Field_Numbers, device_name[0], device_keywords[0], data_dir)
#FOIDEVExtract2(FOIDEV_files, FOIDEV_Field_Numbers, device_name[2], ['MHX'], data_dir)


####### Cross-match the MDRFOI and FOIDEV records 
AllCounts = MAUDE_Merge_Tables(end_year, FOIDEV_files, MDRFOI_files, FOIDEV_Field_Numbers,
                   MDRFOI_Field_Numbers, device_name[0], data_dir)

print('\n')
print('Check the reports that are not from intuitive to make sure they are related to da Vinci')
print('The report 2222833 is manually added in order to compare with cardiac surgery records')
