# Will get the time between failure events, arithmetic mean, and Laplace Test
import urllib, urllib2, cookielib
import BeautifulSoup
import csv
import time
import datetime as dt
from datetime import date
from dateutil import parser
import math
import sys
import re
import nltk
from nltk.probability import ConditionalFreqDist
from nltk.tokenize import word_tokenize
from nltk.stem.wordnet import WordNetLemmatizer
from nltk.corpus import stopwords
from nltk.tag import pos_tag 
import string
import xlwt
import xlrd
from pylab import *
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import matplotlib.ticker as ticker
import pandas as pd
import numpy as np
import scipy as sp
import scipy.stats
from scipy.stats import cumfreq
from scipy import polyval, polyfit
from scipy.interpolate import interp1d
import operator

Excel_In = './daVinci_MAUDE_Data_2013.xls'
Excel_In2 = './daVinci_MAUDE_Classified_2000_2013.xls'
CSV_TTE ='./All_Failure_Times_Classes.csv'
CSV_Out ='Results.csv'

def get_ttf(time_filename, class_filename, out_filename):
    # Prepare the output file    
    f1 = open(out_filename, 'wb')
    csv_wr = csv.writer(f1, dialect='excel', delimiter=',')
    fieldnames = ['MDR_Key', 'Narrative', 'Event','Patient Impact', 'Outcome', 'Corrected_Event_Year', 'Report_Year',
                  'Fallen', 'System Error', 'Moved', 'Arced','Broken', 'Tip Cover', 'Vision',
                  'Surgery_Type', 'Surgery_Class', 'New Converted', 'New Rescheduled', 'System Reset']
    csv_wr.writerow(['MDR_Key','Event_Date','Report_Date',
                     'Report_to_Manufacturer','Date_Received','Date_Manufactured','TTE']+fieldnames[1:])
    
    # Get the malfunction clases and store them in a hash
    databook = xlrd.open_workbook(class_filename)
    datasheet = databook.sheet_by_name('sheet1')
    data_cols = datasheet.ncols
    data_rows = datasheet.nrows
    fieldindex = {'MDR_Key':0}
    # Find the column index of each field in the class file
    for i in range(0, data_cols):
        col = datasheet.cell_value(0, i)
        for f in fieldnames:
            if (col == f):
                fieldindex[f] = i;
    # Make a hash of the malfunction classes found
    classHash = {'MDR_Key':[]}
    for i in range(1, data_rows):
        MDR_Key = str(datasheet.cell_value(i, fieldindex['MDR_Key']));
        row = []
        for f in fieldnames[1:]:
            row.append(str(datasheet.cell_value(i, fieldindex[f])));
        classHash[MDR_Key] = row;
    print 'Hashed the malfunction classes..'

    # Get the field indices for time information
    databook = xlrd.open_workbook(time_filename)
    datasheet = databook.sheet_by_name('Maude_Data')
    data_cols = datasheet.ncols
    data_rows = datasheet.nrows    
    # Find the column numbers for the time information
    for i in range(0, data_cols):
        col = datasheet.cell_value(0, i)
        if (col == 'MDR_REPORT_KEY'):
            MDR_Key_Index = i;
        elif(col == 'DATE_OF_EVENT'):
            Event_Date_Index = i        
        elif(col == 'DATE_REPORT'):
            Report_Date_Index = i
        elif(col == 'DATE_REPORT_TO_MANUFACTURER'):
            ToManufacture_Date_Index = i        
        elif(col == 'DATE_RECEIVED'):
            Received_Date_Index = i
        elif(col == 'DEVICE_DATE_OF_MANUFACTURE'):
            Manufactured_Date_Index = i 

    start_year = 2000
    end_year = 2013
    # Start point
    Start_DateStr = "1/1/"+str(start_year);
    Start_Date = parser.parse(Start_DateStr);
    # End of observation period: 31 December, 2013
    End_DateStr = "12/31/"+str(end_year);
    End_Date = parser.parse(End_DateStr);
    
    All_TTES = []  
    for i in range(1, data_rows):
        Time_to_Event = -1;  
        TTES = []
        MDR_Key = str(datasheet.cell_value(i, MDR_Key_Index))
        Event_DateStr = str(datasheet.cell_value(i, Event_Date_Index))
        Report_DateStr = str(datasheet.cell_value(i, Report_Date_Index))
        ToManufacture_DateStr = str(datasheet.cell_value(i, ToManufacture_Date_Index))
        Received_DateStr = str(datasheet.cell_value(i, Received_Date_Index))
        Manufactured_DateStr = str(datasheet.cell_value(i, Manufactured_Date_Index))

        # If date of event is available and is after the start year
        if (Event_DateStr != ''):
            Event_Date = parser.parse(Event_DateStr);
            if (Event_Date.year > start_year-1) and (Event_Date.year < end_year + 1):
                #print Event_Date.year
                Time_to_Event = (Event_Date - Start_Date).days;
                #print Time_to_Event;
        # Else chose the minimum of other dates as an approximate of date of event
        else:
            Other_dates = []
            # Find the minimum time to date
            for dStr in [Report_DateStr, ToManufacture_DateStr, Received_DateStr]:           
                if (dStr != ''):
                    dDate = parser.parse(dStr);
                    Other_dates.append([dDate.year, dDate, dStr]);
            # If there is any other dates available, chose the min as the time to event
            if Other_dates:
                MinDate = min(Other_dates)
                if (MinDate[0] > start_year - 1) and (MinDate[0] < end_year + 1):
                    #print min(Other_dates)[0]
                    Time_to_Event = (MinDate[1] - Start_Date).days;
                    Event_DateStr = MinDate[2];      
                    #print Time_to_Event;

        # If there exists an event date or can be estimated from other dates
        if (Time_to_Event != -1):
            # Write the output, merge time and class information      
            TTES = [MDR_Key, Event_DateStr, Report_DateStr, ToManufacture_DateStr,
                    Received_DateStr, Manufactured_DateStr, Time_to_Event];
            if (classHash.has_key(MDR_Key)):
                # Corrected Event Year = Based on the minimum date entered 
                classHash[MDR_Key][4] = parser.parse(Event_DateStr).year
                csv_wr.writerow(TTES+classHash[MDR_Key])
                All_TTES.append(TTES+classHash[MDR_Key])
            else:
                print 'Errr: MDR Key not found!'
    print 'Got the failure times..'
    return All_TTES;

def TBF_Analysis(TTES):
    TBFS = []
    Pre_TTE = 0;
    TBF_Avg_Arr = [];
    Laplace = 0;
    Laplace_Arr = [];
    
    for t in TTES:
        # Calculate interarrival time and update the array
        TBFS.append(t - Pre_TTE);
        Pre_TTE = t;

        # Calculate the arithmatic mean of interarrival times
        TBF_Avg_Arr.append((float(1)/float(TTES.index(t)+1))*sum(TBFS[0:]))

        # Calculate the Laplpace Factor
        i = TTES.index(t)+1
        if i > 1:
            sum1 = [];
            for n in range(1,i):
                sum1.append(sum(TBFS[0:n]))
            Laplace = ((float(1)/float(i-1))*sum(sum1)-(float(t)/float(2)))/(float(t)*math.sqrt(float(1)/float(12*(i-1))))        
        Laplace_Arr.append(Laplace)
    print 'Calculated Mean TBF and Laplace Factor..'
    return [TBFS, TBF_Avg_Arr, Laplace_Arr]   

def FC_Analysis(Dates):
    return -1

    
def date_plot(t, y, x_label, y_label, filename):
    t = [dt.datetime.strptime(ti,'%m/%d/%Y').date() for ti in t]
    figure()
    scatter(t, y, facecolors='none')
    gca().xaxis.set_major_locator(mdates.YearLocator())
    gca().xaxis.set_minor_locator(mdates.YearLocator(1,month=7, day =2))
    gca().xaxis.set_major_formatter(ticker.NullFormatter())
    gca().xaxis.set_minor_formatter(mdates.DateFormatter('%Y'))
    plt.rcParams['xtick.minor.size'] = 0
    plt.rcParams['xtick.minor.width'] = 0
    xlim(min(t), max(t))
    ylim(min(y)-1, max(y)+1)
    xlabel(x_label)
    ylabel(y_label)
    #title(g_title)
    grid(True)
    savefig(filename)
    print 'Plotted graphs..'

# Get all the data: time to failures and classes
TTES = get_ttf(Excel_In, Excel_In2, CSV_TTE);

# Read Event Data
df = pd.read_csv(CSV_TTE);
print df.head();

# Start point
Start_DateStr = "01/01/2004";
Start_Date = parser.parse(Start_DateStr);
# End of observation period: 31 December, 2013
End_DateStr = "12/31/2013";
End_Date = parser.parse(End_DateStr);
Num_Days = (End_Date - Start_Date).days+1;
# Number of procedures per year
Num_Proc = [15625,  21052,  42105,  71052, 114814,  170370, 229629, 292000, 367000, 422000]
yy = linspace(1, len(Num_Proc), len(Num_Proc))
dd = linspace(0.5, len(Num_Proc)+0.5, Num_Days)
# Fit a 2nd degree polynomial 
params = polyfit(yy, Num_Proc, 4)
# Predict the number of procedures per day over the whole period
pred_yy = polyval(params,dd)
#pred_yy = sp.interpolate.interp1d(yy, Num_Proc, kind='quadratic')(dd)
#scatter(yy, Num_Proc)
#plot(dd, pred_yy)
#xlim(0,len(Num_Proc)+2)


# Show the estimated number of procedures, don't show for before 2004
min_xlim = dt.datetime.strptime('01/01/2004','%m/%d/%Y').date()
figure();
# Mid-year points
Years = ['2004','2005','2006','2007','2008','2009','2010','2011','2012','2013']
t1=[]
t0=[]
Year_Dates = []
for y in Years:
    Year_Dates.append(dt.datetime.strptime('12/31/'+y,'%m/%d/%Y').date())
    t1.append(dt.datetime.strptime('07/02/'+y,'%m/%d/%Y').date())
    t0.append(dt.datetime.strptime('01/01/'+y,'%m/%d/%Y').date())

ti = dt.datetime.strptime('01/01/2009','%m/%d/%Y')
example_t0 = []
example_h = []
for i in range(1,13):
    example_t0.append(ti.date());
    ti = ti + datetime.timedelta(31)
    example_h.append(pred_yy[(ti-Start_Date).days])
    
t2 = []
ti = Start_Date.date()
End_Datetime = End_Date.date()
while ti <= End_Datetime:    
    t2.append(ti);
    ti = ti+datetime.timedelta(1)        
scatter(t1, Num_Proc, facecolors='none')
bar(t0, Num_Proc,365, alpha=0.5,color='b')
bar(example_t0, example_h,float(365)/float(12),color='b')
annotate('Area under fitted curve = \n Total number of procedures',
         xy=(mdates.date2num(t2[1930]), 150020),xytext=(mdates.date2num(t2[480]), 239950),
         bbox=dict(boxstyle="round", fc="w"),arrowprops=dict(arrowstyle="->",connectionstyle="arc3"),
         color='black',fontsize =13)
plot(t2, pred_yy)
gca().xaxis.set_major_locator(mdates.YearLocator())
gca().xaxis.set_minor_locator(mdates.YearLocator(1,month=7, day =2))
gca().xaxis.set_major_formatter(ticker.NullFormatter())
gca().xaxis.set_minor_formatter(mdates.DateFormatter('%Y'))
plt.rcParams['xtick.minor.size'] = 0
plt.rcParams['xtick.minor.width'] = 0
xlim(min_xlim, max(t2))
ylim(0, max(pred_yy))
yticklabels = [str(p/1000) for p in list(plt.yticks()[0])]
plt.yticks(list(plt.yticks()[0]), yticklabels)
xlabel('Year', fontsize =14)
ylabel('Number of Procedures (Thousands)',fontsize = 14)
grid(True)
savefig('Estimate_Procedures.eps', formar = 'eps', dpi = 1000)
show()

Num_Proc_pred = [];
Test_Sum = 0;
Year_Procs = [];
for d in range(0,Num_Days):
    Num_Proc_pred.append(float(pred_yy[d])/float(365));
    if (d > 1) and (d % 365 == 0):
        Year_Procs.append(Test_Sum);
        Test_Sum = 0;
    else:
        Test_Sum = Test_Sum + Num_Proc_pred[-1];
print Num_Proc
print Year_Procs

seasoncheck = 0;
if seasoncheck == 1:
    # Get event counts per period
    Events = df[['Event_Date','TTE']]
    Sorted_Events = Events.sort(columns = 'TTE', ascending = True)
    print 'Sorted the failure times..'
    Dates = (list(Sorted_Events['Event_Date']))
    # Seasonality Detection
    # Get the average number of reports per different weeks or months of year 
    Period_Sums = {'No': 0}
    for d in Dates:
        if (parser.parse(d).year > Start_Date.year - 1):
            # Period_id = parser.parse(d).isocalendar()[1];
            Period_id = parser.parse(d).month;
            if Period_Sums.has_key(Period_id):
                Period_Sums[Period_id] = Period_Sums[Period_id]+1;
            else:
                Period_Sums[Period_id] = 1;
    Period_Sums.pop('No');
    Period_Avgs = [float(Period_Sums[w])/float(len(Years)) for w in Period_Sums]
    bar(range(1,len(Period_Avgs)+1),Period_Avgs)
    xlim(1, len(Period_Avgs)+1)
    plt.rcParams['xtick.minor.size'] = 0
    plt.rcParams['xtick.minor.width'] = 0
    show()

figure()
Failures_limits = [];
Times_limits = [];
graphs = []
graphs2 = []
labels = ['System Error','Video/Imaging', 'Fallen Pieces', 'Arcing/Broken Tip Covers', 'Broken Instruments']
colors = ['m','b','k','g','r']
markers = ['^','+','*','x','o']
# Four axes, returned as a 2-d array
#f, axarr = plt.subplots(2, 2)
#arr_indices = [[0,0],[0,1],[1,0],[1,1]]
plot_ix = 0;
for malfunc1, malfunc2 in [('System Error','System Error'),('Vision','Vision'),('Arced','Tip Cover'),('Fallen','Fallen'),('Broken','Broken')]:
    Failed = df.ix[df[malfunc1]>0 ,['Event_Date','TTE']]
    if not (malfunc2 == malfunc1):
        Failed = Failed.append(df.ix[df[malfunc2]>0 ,['Event_Date','TTE']])
    Failed[['TTE']] = Failed[['TTE']].astype(int)
    print 'Got '+ str(len(Failed)) +' failure times..'
    Sorted_Failed = Failed.sort(columns = 'TTE', ascending = True)
    print 'Sorted the failure times..'
    Dates = (list(Sorted_Failed['Event_Date']))
 
    # Get failure counts per day
    Failures_Years = [0,0,0,0,0,0,0,0,0,0]
    Failure_dict = {'Date': [-1, -1]}
    for d in Dates:        
        y = parser.parse(d).year        
        if (y >= Start_Date.year):
            Failures_Years[y-2004] = Failures_Years[y-2004]+1; 
            if Failure_dict.has_key(d):
                Failure_dict[d][0] = Failure_dict[d][0]+1;
            else:
                Failure_dict[d] = [1,-1];
    Failure_dict.pop('Date');

    Cum_Failures_Years = [0];
    for y in Failures_Years:
        Cum_Failures_Years.append(Cum_Failures_Years[-1] + (float(y)/float(Num_Proc[Failures_Years.index(y)])));
        #Cum_Failures_Years.append(Cum_Failures_Years[-1] + (float(y)));
    Cum_Failures_Years = Cum_Failures_Years[1:]

    # Get the number of failure per procedure for each week
    Num_Proc_Week = 0;
    Num_Failure_Week = 0;
    Num_Failure_Month = 0;
    Procs_Weeks = []
    AbFailures_Weeks = []
    AbFailures_Months = []
    Failures_Weeks = []
    Week_Dates = []
    Month_Dates = []
    Cum_Failures_Weeks = [0];
    ti = Start_Date;
    dateIndex = 0;
    Month_Start = ("01/01/2004");
    while ti <= End_Date:       
        # Every first of month, calculate the failures for the previous month, save and reset
        if (ti > Start_Date) and (ti.day == 1):
            AbFailures_Months.append(Num_Failure_Month)
            Month_Dates.append(Month_Start)
            Month_Start = ti.strftime("%m/%d/%Y");
            Num_Failure_Month = 0;
        # Every monday, calculate the failures per procedure for the previous week, save, and reset
        if (ti.weekday() == 0):
            AbFailures_Weeks.append(Num_Failure_Week)
            Procs_Weeks.append(Num_Proc_Week)
            Failures_Weeks.append((float(Num_Failure_Week)/float(Num_Proc_Week)));
            #Failures_Weeks.append((float(Num_Failure_Week)));
            Cum_Failures_Weeks.append(Cum_Failures_Weeks[-1] + Failures_Weeks[-1]);
            Previous_monday = (ti-datetime.timedelta(7)).strftime("%m/%d/%Y");
            Week_Dates.append(Previous_monday)
            Num_Proc_Week = 0;
            Num_Failure_Week = 0;
        # Add the number of failures for the current week
        dateStr = ti.strftime("%m/%d/%Y");
        if Failure_dict.has_key(dateStr):
            Num_Failure_Week = Num_Failure_Week + Failure_dict[dateStr][0];
            Num_Failure_Month = Num_Failure_Month + Failure_dict[dateStr][0];
        # Add the number of procedures for the current week
        #print Num_Proc_pred[dateIndex]
        if (Num_Proc_pred[dateIndex]) > 0:
            Num_Proc_Week = Num_Proc_Week + Num_Proc_pred[dateIndex];
        else:
            print Num_Proc_Week            
        # Goto the next day
        dateIndex = dateIndex+1;
        ti = ti+datetime.timedelta(1)   
    #Cum_Failures_Weeks = [float(f)/float(Cum_Failures_Weeks[-1]) for f in Cum_Failures_Weeks[1:]]
    Cum_Failures_Weeks = Cum_Failures_Weeks[1:]

    Norm_Line = [];
    Incr_Line1 = [];
    Incr_Line2 = [];
    Decr_Line = [];
    t = 0;
    f1 = open('./'+malfunc1+'_'+CSV_Out, 'wb')
    csv_wr = csv.writer(f1, dialect='excel', delimiter=',')
    csv_wr.writerow(['Week No.', 'Starting Date', 'No. Failures', 'No. Procedures','No. Failures per Procedures' , 'Cum Failures','Constant Rate Line'])
    for i in range(0,len(Failures_Weeks)):
        t = t + float(1)/float(len(Failures_Weeks));
        # Constant rate        
        Norm_Line.append(t);
        # Increasing Rate
        v = float(3)
        a = float(2) 
        Incr_Line1.append(float(v)*t+(float(a)/float(2))*t*t);
        v = float(2)
        a = float(3.5) 
        Incr_Line2.append(float(v)*t+(float(a)/float(2))*t*t);
        # Decreasing Rate
        v = float(1)/float(2)
        a = float(-1)/float(2) 
        Decr_Line.append(float(v)*t+(float(a)/float(2))*t*t);
        
        csv_wr.writerow([i+1, Week_Dates[i],AbFailures_Weeks[i], Procs_Weeks[i],Failures_Weeks[i],Cum_Failures_Weeks[i],Norm_Line[-1]])
    f1.close();
    
    # Plot the current ax
    Week_Dates = [dt.datetime.strptime(ti,'%m/%d/%Y').date() for ti in Week_Dates]
    #current_ax = axarr[arr_indices[plot_ix][0],arr_indices[plot_ix][1]]
    #graphs.append(current_ax.scatter(Week_Dates,Cum_Failures_Weeks,
    #                                marker = markers[plot_ix],edgecolors = colors[plot_ix], facecolors='none'))
    #current_ax.plot(Week_Dates,Norm_Line)  
    #current_ax.set_title(labels[plot_ix])     

    plt.figure(1)
    graphs.append(scatter(Week_Dates,Cum_Failures_Weeks,
                  marker = markers[plot_ix],s= 8, edgecolors = colors[plot_ix], facecolors='none'))

    Month_Dates = [dt.datetime.strptime(ti,'%m/%d/%Y').date() for ti in Month_Dates]
    plt.figure(2)
    graphs2.append(plot(Month_Dates,AbFailures_Months))
    #graphs2.append(plot(Cum_Failures_Years))
   
    # Set the graph limits            
    if Times_limits:
        if min(Week_Dates) < Times_limits[0]:
            Times_limits[0] = min(Week_Dates)
        if max(Week_Dates) > Times_limits[1]:
            Times_limits[1] = max(Week_Dates)
    else:
        Times_limits.append(min(Week_Dates))
        Times_limits.append(max(Week_Dates))
        
    if Failures_limits:        
        if min(Cum_Failures_Weeks) < Failures_limits[0]:
            Failures_limits[0] = min(Cum_Failures_Weeks)
        if max(Cum_Failures_Weeks) > Failures_limits[1]:
            Failures_limits[1] = max(Cum_Failures_Weeks)    
    else:
        Failures_limits.append(min(Cum_Failures_Weeks))
        Failures_limits.append(max(Cum_Failures_Weeks))
     
    # Set the labels and ticks
    #current_ax.xaxis.set_major_locator(mdates.YearLocator())
    #current_ax.xaxis.set_minor_locator(mdates.YearLocator(1,month=7, day =2))
    #current_ax.xaxis.set_major_formatter(ticker.NullFormatter())
    #current_ax.xaxis.set_minor_formatter(mdates.DateFormatter('%Y'))
    #current_ax.set_xlim(Times_limits[0], Times_limits[1])
    #current_ax.set_ylim(Failures_limits[0], Failures_limits[1])
    #plt.setp(current_ax, yticks=[0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,0.8,0.9,1])
    #if (plot_ix == 2 or plot_ix == 3):
    #    current_ax.set_xlabel('Year')
    #if (plot_ix == 0 or plot_ix == 2):
    #    current_ax.set_ylabel('Cumulative Malfunction Rate per Procedure')
    #current_ax.grid(True)
    plot_ix = plot_ix + 1;

plt.figure(1)
plot(Week_Dates,Norm_Line,'k--')
#plot(Week_Dates,Incr_Line1,'k--')
plot(Week_Dates,Incr_Line2,'k--')
plot(Week_Dates,Decr_Line,'k--')
#legend(graphs,labels,loc=2)
gca().xaxis.set_major_locator(mdates.YearLocator())
gca().xaxis.set_minor_locator(mdates.YearLocator(1,month=7, day =2))
gca().xaxis.set_major_formatter(ticker.NullFormatter())
gca().xaxis.set_minor_formatter(mdates.DateFormatter('%Y'))
plt.rcParams['xtick.minor.size'] = 0
plt.rcParams['xtick.minor.width'] = 0
xlim(Times_limits[0], Times_limits[1])
ylim(Failures_limits[0], Failures_limits[1])
xlabel('Year',fontsize = 14)
ylabel('Cumulative Number of Malfunctions per Procedure',fontsize = 14)
grid(True)
labels = ['System Error','Video/Imaging', 'Fallen Pieces', 'Arcing/Broken Tip Covers', 'Broken Instruments']
colors = ['magenta','blue','black','green','red']
xlocs = [0,0,0,0,0]; ylocs = [0,0,0,0,0];
rots = [2,0,14,13,0]
xlocs[0] = 415; ylocs[0] = 0.27;
xlocs[1] = 405; ylocs[1] = 0.13;
xlocs[2] = 110; ylocs[2] = 0.52;
xlocs[3] = 320; ylocs[3] = 0.56;
xlocs[4] = 340; ylocs[4] = 1.1;
for plot_ix in range(0,len(labels)):
    a = annotate(labels[plot_ix], (mdates.date2num(Week_Dates[xlocs[plot_ix]]), ylocs[plot_ix]),
             bbox=dict(boxstyle="round", ec = colors[plot_ix], fc="w"),xycoords='data',color=colors[plot_ix], rotation=rots[plot_ix],fontsize = 13)
    a.get_bbox_patch().set_boxstyle("round,pad=0.15")

a = annotate('Constant Rate', xy=(mdates.date2num(Week_Dates[420]), 0.8),xytext=(mdates.date2num(Week_Dates[310]), 0.88),
        bbox=dict(boxstyle="round", ec = "w",fc="w"),arrowprops=dict(arrowstyle="->",connectionstyle="arc3"),color='black',fontsize =12)
a.get_bbox_patch().set_boxstyle("round,pad=0.15")

a = annotate('Increasing Rate', xy=(mdates.date2num(Week_Dates[170]), 0.82),xytext=(mdates.date2num(Week_Dates[60]), 0.9),
        bbox=dict(boxstyle="round", ec = "w",fc="w"),arrowprops=dict(arrowstyle="->",connectionstyle="arc"),color='black',fontsize =12)
a.get_bbox_patch().set_boxstyle("round,pad=0.15")

a = annotate('Decreasing Rate', xy=(mdates.date2num(Week_Dates[270]), 0.2),xytext=(mdates.date2num(Week_Dates[270]), 0.26),
        bbox=dict(boxstyle="round", ec = "w",fc="w"),arrowprops=dict(arrowstyle="->",connectionstyle="arc3"),color='black',fontsize =12)
a.get_bbox_patch().set_boxstyle("round,pad=0.15")

savefig('CDF_Failures.eps', formar = 'eps', dpi = 1000)
show()
            
