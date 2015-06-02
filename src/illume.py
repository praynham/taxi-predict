# ILLUME - Explore Porto taxi-trip data file - Python

# Functions for exploring and analyzing the taxi-trip CSV data file
# that was provided as part of the Kaggle competition 2015.
#
# Main functions:
#     illume - display selected trip entries in readable format
#     summary - create a CSV file of summary data about each trip
#     prepare - create a CSV file for further statistical processing
#
# Sample use:
#
# >>> illume( 'D:/CKME 136/Taxi/train.csv', 20 )

import sys
import csv
from math import *
import tkinter as tk
import datetime as dt

# GLOBAL CONSTANT

COMMANDED = False   # Is this script run as a stand-alone command?

# ILLUME - Display taxi data in human-comprehensible form
#
# Parameters:
#     filename - name of file containing taxi data (in CSV format)
#     limit - not 0 => print up to 'limit' number of entries
#         0 => print entries 1, 2, 3, 4, 6, 8, 12, 16, 24, 32, 48, ...
#     start - if limit is not 0 => start printing at entry 'start'
#     hasHead - True => taxi data file has a header row
#
# Notes:
#     Use 'limit=0' if you don't know how big the file is and just
#     want to see a reasonable-size selection from throughout the file.
#
#     If the file has a header row, the header values are used as labels
#     for the data fields in the printed output. If there is no header row,
#     then the data fields are labeled by position: 1, 2, 3, etc.

def illume( filename, limit=10, start=0, hasHead=True ):
    source = open( filename, 'r' )
    table = csv.reader( source )
    labels = [ ]
    labelwidth = 3
    finis = start + limit
    count = 0
    for line in table:
        if len(labels) == 0 and hasHead:
            labels = line
            labelwidth = max(( len(label) for label in labels ))
            labelwidth = max( 3, labelwidth )
        else:
            if (limit != 0 and count >= start and count < finis
            or limit == 0 and isInteresting(count)):
                print( "__", count, "_"*labelwidth, sep="" )
                while len(labels) < len(line):
                    labels .append( str(len(labels)+1) )
                for (label,atom) in zip( labels, line ):
                    if label == 'POLYLINE':
                        waypoints = parseWaypoints( atom )
                        printWaypoints( label, waypoints, indent=labelwidth )
                        drivedist = sum( p[2] for p in waypoints )
                        tripdist = geodist(
                                waypoints[0][0], waypoints[0][1],
                                waypoints[-1][0], waypoints[-1][1])
                        triptime = max( 0, len(waypoints)-1 ) * 0.25
                        print( '{:s}: {:6.3f} {:s}'.format(
                                "ROUTE_LEN".rjust(labelwidth), drivedist/1000, 'km' ))
                        print( '{:s}: {:6.3f} {:s}'.format(
                                "TRIP_LEN".rjust(labelwidth), tripdist/1000, 'km' ))
                        print( '{:s}: {:5.2f} {:s}'.format(
                                "TRIP_TIME".rjust(labelwidth), triptime, 'min' ))
                        print( '{:s}: {:4.1f} {:s}'.format(
                                "AVG_SPEED".rjust(labelwidth),
                                drivedist/triptime*(60/1000) if triptime!=0 else 0,
                                'km/h' ))
                        print( '{:s}: {:4.1f} {:s}'.format(
                                "TRIP_SPEED".rjust(labelwidth),
                                tripdist/triptime*(60/1000) if triptime!=0 else 0,
                                'km/h' ))
                    else:
                        suffix = annotate( label, atom )
                        if suffix != '':
                            suffix = '  (' + suffix + ')'
                        print( '{:s}: {:s}{:s}'.format(
                                label.rjust(labelwidth), atom, suffix ))
                print()
            count += 1
        if limit != 0 and count >= finis: break
    if count > 0:
        print( "_" * (labelwidth+2) )
    source.close()
    return count

# RAW SAMPLE - Create a file with sample records of raw data
#
# Create a sample data file using a subset of entries from a source file.
#
# Parameters:
#     filename - name of source data file
#     samplename - name of sample data file to receive subset
#     limit - not 0 => write 'limit' number of records to sample file
#         is 0 => write a selection of records from througout the file
#     start - start extracting at this record
#     hasHead - True => first row of file is a header, always copied to output

def rawsample( filename, samplename, limit=10, start=0, hasHead=True ):
    source = open( filename, 'r' )
    sample = open( samplename, 'w' )
    finis = start + limit
    count = 0
    didHead = False
    for line in source:
        if hasHead and not didHead:
            sample.write( line )
            didHead = True
        else:
            if (limit != 0 and count >= start and count < finis
            or limit == 0 and isInteresting(count)):
                sample.write( line )
            count += 1
        if limit != 0 and count >= finis: break
    sample.close()
    source.close()
    return count

# SUMMARIZE - Create a summary flat-file from the taxi data file
#
# Essentially, create a flat file (no repeating group) of taxi rides
# from the taxi file. Do this by replacing the 'polyline' field (which
# is a list of points) with three atomic fields: 'drive dist' (length
# of the route the taxi drives, in km), 'trip dist' (linear distance
# from the starting point to the ending point, in km), and 'trip time'
# (total time of the trip, in minutes).
#
# Parameters:
#    fileName - name of input file of taxi data
#    summName - name of output file to receive summary data
#    limit - maximum number of entries to write to the summary file
#    sample - write only every 1/sample entries to the summary file,
#        e.g., sample=100 makes a 1% sample of the source data
#    hashead - True => both input and output files have header rows;
#        False => neither the source nor the summary have header rows.

def summarize( fileName, summName, limit=0, sample=1, hasHead=True ):
    source = open( fileName, 'r' )
    summary = open( summName, 'w' )
    table = csv.reader( source )
    labels = [ ]
    count = 0
    for line in table:
        if len(labels) == 0 and hasHead:
            labels = line
            labels1 = line[0:8]
            labels1.append("DRIVE_DIST")
            labels1.append("TRIP_DIST")
            labels1.append("TRIP_TIME")
            summary.write( ",".join(labels1)  )
            summary.write( "\n" )
        else:
            if count % sample == 0:
                for (label,atom) in zip( labels, line ):
                    if label == 'POLYLINE':
                        waypoints = parseWaypoints( atom )
                        drivedist = sum( p[2] for p in waypoints )
                        tripdist = 0 if len(waypoints)==0 else geodist(
                                waypoints[0][0], waypoints[0][1],
                                waypoints[-1][0], waypoints[-1][1])
                line1 = line[0:7]
                line1.append(
                        "True" if len(waypoints)==0 else
                        "True" if len(line)<7 or line[7][0:1]=='T' else
                        "False" )
                line1.append( "{:.3f}".format( drivedist/1000 ))
                line1.append( "{:.3f}".format( tripdist/1000 ))
                line1.append( "{:.2f}".format( 0.25 * max( 0, len(waypoints)-1 )))
                summary.write( ",".join(line1)  )
                summary.write( "\n" )
        count += 1
        if count == limit: break
        if isInteresting( count ):
            print( "At", count )
    source.close()
    summary.close()
    print( "End at", count )
    return count

# PREPARE - Prepare data for statistical analysis
#
# Parameters:
#    fileName - name of input file of taxi data
#    prepName - name of output file to receive summary data
#    limit - maximum number of entries to write to the summary file
#    sample - write only every 1/sample entries to the summary file,
#        e.g., sample=100 makes a 1% sample of the source data
#    hashead - True => both input and output files have header rows;
#        False => neither the source nor the summary have header rows.
#
# Input file:
#     CSV data of taxi trips in standard Porto format
#
# Output file:
#     CSV data file of all or a subset of the taxi trips, with these columns:
#         Trip Id - First seven fields, same as source data file
#         Call Type
#         Origin Call
#         Origin Stand
#         Taxi Id
#         Timestamp
#         Day Type
#         Week_Day - Day of week, 0..6 for Sun..Sat
#         Day_Busy - Type of day, WD=workday, WE=weekend, HOL=holiday
#         Day_Hour - Time of day in hours, bounded 4..28 (4AM to 4AM)
#         Missing Data - True if GPS trip data was incomplete
#         Drive Dist - Distance that taxi drove, in metres
#         Trip Dist - Linear distance from start point to end point
#         Trip Time - Duration of trip, in minutes
#         Lon Start - Longitude of starting point
#         Lat Start - Latitude of starting point
#         Lon 02 - Longitude at 2 minutes into the trip
#         Lat 02 - Latitude at 2 minutes into the trip
#         Lon 05 - at 5 minutes
#         Lat 05
#         Lon 10 - at 10 minutes
#         Lat 10
#         Lon Finish - at the finish of the trip
#         Lat Finish
#
#     If the trip took less than 10 minutes, some Lon/Lat fields are blank.

def prepare( fileName, prepName, limit=0, sample=1, hasHead=True ):
    sample = int( max( 1, sample ))
    source = open( fileName, 'r' )
    destiny = open( prepName, 'w' )
    table = csv.reader( source )
    labels = [ ]
    excuses = [0] * 5
    count = 0
    for line in table:
        if len(labels) == 0 and hasHead:
            # The input file has a header row,
            # so write a header row to the output file
            labels = line
            labels1 = line[0:7]
            labels1.extend([
                "WEEK_DAY", "DAY_BUSY", "DAY_HOUR",
                "MISSING_DATA",
                "DRIVE_DIST",
                "TRIP_DIST", "TRIP_TIME",
                "LON_START", "LAT_START",
                "LON_02", "LAT_02",
                "LON_05", "LAT_05",
                "LON_10", "LAT_10",
                "LON_FINISH", "LAT_FINISH" ])
            destiny.write( ",".join(labels1)  )
            destiny.write( "\n" )
        else:
            if count % sample == 0:
                waypoints = []
                drivedist = tripdist = triptime = 0
                # Preprocess the atoms of input.
                # - here only the trip waypoints (polyline) needs work
                for (label,atom) in zip( labels, line ):
                    if label == 'POLYLINE':
                        waypoints = parseWaypoints( atom )
                        drivedist = sum( p[2] for p in waypoints )
                        tripdist = 0 if len(waypoints)==0 else geodist(
                                waypoints[0][0], waypoints[0][1],
                                waypoints[-1][0], waypoints[-1][1])
                        triptime = 0.25 * max( 0, len(waypoints)-1 )
                # Check if this row is an outlier
                # - if it is, we'll ignore it for now
                # - but common outliers we should figure out how to handle
                happy = False
                if triptime < 0.5:
                    # trip too short, less than 30 seconds
                    excuses[1] += 1
                elif tripdist < 30:
                    # trip too short, less than 30 metres distance
                    excuses[2] += 1
                elif drivedist / triptime < 5:
                    # average speed too slow, less than 5 km/h
                    excuses[3] += 1
                elif max( (here[2] for here in waypoints) ) > 625:
                    # speed too high, over 150 km/h (625 m/15s) at some point
                    excuses[4] += 1
                else:
                    # The data looks good, so we are happy
                    happy = True
                    excuses[0] += 1
                # Construct the output row
                # - starting with the first seven input fields verbatim
                line1 = line[0:7]
                timeinfo = decodestamp( line[5] )
                line1.append( str(timeinfo[2]) ) # day of week
                line1.append( str(timeinfo[3]) ) # type of day
                line1.append( "{:.3f}".format(timeinfo[4]) ) # hour of day
                line1.append(
                        "True" if len(waypoints)==0 else
                        "True" if len(line)<7 or line[7][0:1]=='T' else
                        "False" )
                line1.append( "{:.0f}".format( drivedist ))
                line1.append( "{:.0f}".format( tripdist ))
                line1.append( "{:.2f}".format( triptime ))
                for ix in [0, 2*4, 5*4, 10*4, -1]:
                    if -len(waypoints) <= ix < len(waypoints):
                        here = waypoints[ix]
                        line1.append( "{:.6f},{:.6f}". format( here[0], here[1] ))
                    else:
                        line1.append( "," )
                # Write the row
                destiny.write( ",".join(line1)  )
                destiny.write( "\n" )
            count += 1
        if count == limit*sample: break
        if isInteresting( count ):
            print( "At", count )
    source.close()
    destiny.close()
    print( "End at", count )
    if count > 0:
        print( "-", excuses[0], "accepted" )
        print( "-", excuses[1], "ignored - trip less than 30 seconds" )
        print( "-", excuses[2], "ignored - trip less than 30 metres" )
        print( "-", excuses[3], "ignored - taxi averaged under 5 km/h" )
        print( "-", excuses[4], "ignored - taxi exceeded 150 km/h" )
    return count

# CHOOSE FILE - Invite the user to select a file in a chooser window
#
# Use TK windowing toolkit. Need to initialize with 'withdraw' to prevent
# the application from displaying a main window.

tk_root = tk.Tk()
tk_root.withdraw()

def chooseFile():
    return tk.filedialog.askopenfilename()
    
# IS INTERESTING - Return 'true' for a decreasing selection of whole numbers
#
# Return 'true' for the numbers: 1, 2, 3, 4, 6, 8, 12, 16, 24, 32, ...
# I.e., return true for two-powers and sesqui-two-powers.

def isInteresting( num ):
    red = num & (num-1)
    return (red == 0
        or  red & (red-1) == 0
        and num & (num>>1) != 0)

# ANNOTATE - Return an annotation that hopefully clarifies an atom of data

def annotate( label, atom ):
    suffix = ''
    if label == 'CALL_TYPE':
        suffix = ('central dispatch' if atom == 'A'
                else 'taxi stand' if atom == 'B'
                else 'street hail' if atom == 'C'
                else '-' if atom != '' else '')
    elif label == 'DAY_TYPE':
        suffix = ('normal day' if atom == 'A'
                else 'holiday' if atom == 'B'
                else 'day before holiday' if atom == 'C'
                else '-' if atom != '' else '')
    elif label == 'TIMESTAMP':
        if atom.isdigit():
            stamp = dt.datetime.utcfromtimestamp( int(atom) )
            suffix = stamp.strftime( '%b-%d %H:%M:%S' )
    return suffix

# PARSE WAYPOINTS - Compile a list of waypoints from a text string
#
# Parameter:
#    text - list of waypoints, as longitude/latitude pairs, in format:
#        [[lon0,lat0],[lon1,lat1],[lon2,lat2],...]
#
# Result:
#    List of tuples, one for each waypoint. Each tuple contains:
#        longitude - degrees from -180 to 180
#        latitude - degrees from -180 to 180
#        distance - distance from prior waypoint to this one, in metres
#        heading - direction from prior waypoint to this one, in degrees
#            -180=180=east, 90=north, 0=west, -90=south

def parseWaypoints( text ):
    stamps = text .lstrip('[') .rstrip(']') .split('],[')
    if len(stamps)==1 and stamps[0]=="":
        stamps = [ ]
    lastLongi, lastLati = 0, 0
    waypoints = [ ]
    happy = True
    for stamp in stamps:
        (longi, junk, lati) = stamp.partition(',')
        if True:
            try:
                longi, lati = float(longi), float(lati)
            except:
                if longi != '' or lati != '':
                    if happy:
                        print( "Cannot parse trip:", text )
                        happy = False
                longi, lati = 0, 0
            if lastLongi != 0 and lastLati != 0:
                distance = geodist( longi, lati, lastLongi, lastLati )
                heading = geodir( longi, lati, lastLongi, lastLati )
                waypoint = (longi, lati, distance, heading)
                lastLongi, lastLati = longi, lati
            else:
                waypoint = (longi, lati, 0, 0)
                lastLongi, lastLati = longi, lati
        else:
            waypoint = (0, 0, 0, 0)
            lastLongi, lastLati = 0, 0
        waypoints. append( waypoint )
    return waypoints

# PRINT WAYPOINTS - Print a list of waypoints in a nice readable form
#
# Print an line with a label if the label is given, then print one line
# for each waypoint in the list. For each waypoint append a graphic that
# illustrates the direction (compass point) and speed (arrow line ===>).
#
# Parameters:
#     label - optional label to put at the top of the printout;
#     points - list of waypoints as returned by parseWaypoints;
#     indent - number of spaces to indent each waypoint line.

def printWaypoints( label, points, indent=2 ):
    if label:
        print( label.rjust(indent), ':', sep='' )
    prefix = ' ' * indent
    ticks = 0
    for point in points:
        if point[2] != 0:
            print( '{:s}{:5.2f}: {:.6f}, {:.6f}, {:4.0f}m, {:4.0f}°  {:s}{:s}'.format(
                prefix, ticks,
                point[0], point[1],
                point[2], point[3],
                geodirname(point[3]),
                "<" * (-90 < point[3] <= 90) +
                "=" * min(30,round(point[2]/50)) +
                ">" * (not -90 < point[3] <= 90) ))
        else:
            print( '{:s}{:5.2f}: {:.6f}, {:.6f}, {:4.0f}m,    -'.format(
                prefix, ticks,
                point[0], point[1], point[2] ))
        ticks += 0.25

# GEO DIST - return distance (metres) between two points (lon/lat degrees)
# For distances within a city, this procedure delivers with acceptable
# accuracy at less CPU cost than the haversine distance function.

def geodist( lon1, lat1, lon2, lat2 ):
    latFactor = cos( ((lat1+lat2)/2) * (pi/180) )
    # latFactor = 0.75293 # cos(41.155) - quick calc but only for Porto, Portugal
    radius = 6371000
    lonDelta = (lon1 - lon2) * latFactor
    latDelta = lat1 - lat2
    dist = sqrt( lonDelta*lonDelta + latDelta*latDelta ) * radius*(pi/180)
    return dist

# GEO DIR - return direction (degrees) from one point to another (lat/lon degrees)

def geodir( lon1, lat1, lon2, lat2 ):
    latFactor = cos( ((lat1+lat2)/2) * (pi/180) ) # precise calc
    # latFactor = 0.75293 # cos(41.155) - quick calc but only for Porto, Portugal
    lonDelta = (lon1 - lon2) * latFactor
    latDelta = lat1 - lat2
    angle = atan2( latDelta, lonDelta ) * 180/pi
    return angle

# HAVER DIST - return precise distance (m) between two points (lon/lan deg)
#
# HaverDist is accurate for all pairs of points because it accounts for
# Earth's spherical shape. The distance is caculated along a great circle
# that connects the two points.

def haverdist( lon1, lat1, lon2, lat2 ):
    radius = 6371000   # radius of earth in metres
    lon1, lat1 = lon1*pi/180, lat1*pi/180
    lon2, lat2 = lon2*pi/180, lat2*pi/180
    halver = haversin(lat1-lat2) + cos(lat1)*cos(lat2)*haversin(lon1-lon2)
    dist = 2*radius*asin(sqrt(halver))    
    return dist

# HAVER SIN - "Half versed sign" trig function

def haversin( angl ):
    sinx = sin(angl/2)
    return sinx*sinx

# GEO DIR NAME - Return the closest compass-point for an angle (deg)
#
# Use eight-point compass rose. W=0°, N=90°, E=180°/-180°, S=-90°.

def geodirname( angle ):
    names = [ ' W', 'NW', ' N', 'NE', ' E', 'SE', ' S', 'SW' ]
    angle = round( angle*8/360 ) % 8
    return names[angle]

# DECODE STAMP - Extract relevant details from a date/time stamp
#
# Parameter:
#    stamp - Unix date/time stamp (seconds past 1970/1/1 00:00 UTC)
#
# Result:
#    Tuple of five elements:
#        stamp - original timestamp
#        numeric stamp - date as a number of form YYYYMMDD
#        weekday - 0=Sunday, 1=Monday, ... 6=Saturday
#        day type - "WD"=workday, "WE"=weekend, "HOL"=holiday
#        time of day - in hours, e.g. 15.50 = 3:30 PM
#
# We treat the hours 0:00 to 4:00 (midnight through 4 AM) as being part
# of the previous day, and the hours of a day run from 4:00 to 28:00.
# For example, Jan 4 at 1:30 AM is treated as Jan 3 at 25:30. This is
# done to help capture taxi-usage patterns that run across midnight.

# Schedule of holidays comes from http://www.timeanddate.com/holidays/portugal/2014

PortugalHolidays = [
    20130101, 20130329, 20130331, 20130425, 20130501,
    20130610, 20130815, 20131208, 20131225,
    20140101, 20140418, 20140420, 20140425, 20140501,
    20140610, 20140815, 20141208, 20141225 ]

def decodestamp( stamp ):
    when1 = when = dt.datetime.utcfromtimestamp( int(stamp) )
    weekday = when.weekday()
    dayhour = when.hour + when.minute/60 + when.second/3600
    if dayhour < 4:
        dayhour += 24
        weekday = (weekday-1) % 7
        when1 = when - dt.timedelta( hours=24 )
    numstamp = when1.year*10000 + when1.month*100 + when1.day
    daytype = ('WE' if weekday==0 or weekday==6 else
               'HOL' if numstamp in PortugalHolidays else
               'WD')
    return (stamp, numstamp, weekday, daytype,  dayhour)

# MAIN - Display entries from a taxi-trip data file in readable format
#
# Global constant COMMANDED enables this function. Enable this function
# when this script is used as a stand-alone command; disable it when
# this script is used as a workspace within a Python interactive shell.
#
# Command parameters:
#     FileName - name of file holding taxi-trip data
#     Count - not 0 => display first 'Count' entries;
#         is 0 => display selected entries from throughout the file.

if __name__ == "__main__" and COMMANDED:
    args = sys.argv
    if len(args) < 1:
        print( "Usage: illume FileName.csv [Count]" )
        exit( 2 )
    else:
        illume( args[0], args[1] if len(args)>=2 else 10 )

