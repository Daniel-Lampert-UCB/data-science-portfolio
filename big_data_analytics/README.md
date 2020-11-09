# Project 1: Query Project

- In the Query Project, you will get practice with SQL while learning about
  Google Cloud Platform (GCP) and BiqQuery. You'll answer business-driven
  questions using public datasets housed in GCP. To give you experience with
  different ways to use those datasets, you will use the web UI (BiqQuery) and
  the command-line tools, and work with them in Jupyter Notebooks.

#### Problem Statement

- You're a data scientist at Lyft Bay Wheels (https://www.lyft.com/bikes/bay-wheels), formerly known as Ford GoBike, the
  company running Bay Area Bikeshare. You are trying to increase ridership, and
  you want to offer deals through the mobile app to do so. 
  
- What deals do you offer though? Currently, your company has several options which can change over time.  Please visit the website to see the current offers and other marketing information. Frequent offers include: 
  * Single Ride 
  * Monthly Membership
  * Annual Membership
  * Bike Share for All
  * Access Pass
  * Corporate Membership
  * etc.

- Through this project, you will answer these questions: 

  * What are the 5 most popular trips that you would call "commuter trips"? 
  
  * What are your recommendations for offers (justify based on your findings)?

- Please note that there are no exact answers to the above questions, just like in the proverbial real world.  This is not a simple exercise where each question above will have a simple SQL query. It is an exercise in analytics over inexact and dirty data. 

- You won't find a column in a table labeled "commuter trip".  You will find you need to do quite a bit of data exploration using SQL queries to determine your own definition of a communter trip.  In data exploration process, you will find a lot of dirty data, that you will need to either clean or filter out. You will then write SQL queries to find the communter trips.

- Likewise to make your recommendations, you will need to do data exploration, cleaning or filtering dirty data, etc. to come up with the final queries that will give you the supporting data for your recommendations. You can make any recommendations regarding the offers, including, but not limited to: 
  * market offers differently to generate more revenue 
  * remove offers that are not working 
  * modify exising offers to generate more revenue
  * create new offers for hidden business opportunities you have found
  * etc. 

#### All Work MUST be done in the Google Cloud Platform (GCP) / The Majority of Work MUST be done using BigQuery SQL / Usage of Temporary Tables, Views, Pandas, Data Visualizations

A couple of the goals of w205 are for students to learn how to work in a cloud environment (such as GCP) and how to use SQL against a big data data platform (such as Google BigQuery).  In keeping with these goals, please do all of your work in GCP, and the majority of your analytics work using BigQuery SQL queries.

You can make intermediate temporary tables or views in your own dataset in BigQuery as you like.  Actually, this is a great way to work!  These make data exploration much easier.  It's much easier when you have made temporary tables or views with only clean data, filtered rows, filtered columns, new columns, summary data, etc.  If you use intermediate temporary tables or views, you should include the SQL used to create these, along with a brief note mentioning that you used the temporary table or view.

In the final Jupyter Notebook, the results of your BigQuery SQL will be read into Pandas, where you will use the skills you learned in the Python class to print formatted Pandas tables, simple data visualizations using Seaborn / Matplotlib, etc.  You can use Pandas for simple transformations, but please remember the bulk of work should be done using Google BigQuery SQL.

#### GitHub Procedures

In your Python class you used GitHub, with a single repo for all assignments, where you committed without doing a pull request.  In this class, we will try to mimic the real world more closely, so our procedures will be enhanced. 

Each project, including this one, will have it's own repo.

Important:  In w205, please never merge your assignment branch to the master branch. 

Using the git command line: clone down the repo, leave the master branch untouched, create an assignment branch, and move to that branch:
- Open a linux command line to your virtual machine and be sure you are logged in as jupyter.
- Create a ~/w205 directory if it does not already exist `mkdir ~/w205`
- Change directory into the ~/w205 directory `cd ~/w205`
- Clone down your repo `git clone <https url for your repo>`
- Change directory into the repo `cd <repo name>`
- Create an assignment branch `git branch assignment`
- Checkout the assignment branch `git checkout assignment`

The previous steps only need to be done once.  Once you your clone is on the assignment branch it will remain on that branch unless you checkout another branch.

The project workflow follows this pattern, which may be repeated as many times as needed.  In fact it's best to do this frequently as it saves your work into GitHub in case your virtual machine becomes corrupt:
- Make changes to existing files as needed.
- Add new files as needed
- Stage modified files `git add <filename>`
- Commit staged files `git commit -m "<meaningful comment about your changes>"`
- Push the commit on your assignment branch from your clone to GitHub `git push origin assignment`

Once you are done, go to the GitHub web interface and create a pull request comparing the assignment branch to the master branch.  Add your instructor, and only your instructor, as the reviewer.  The date and time stamp of the pull request is considered the submission time for late penalties. 

If you decide to make more changes after you have created a pull request, you can simply close the pull request (without merge!), make more changes, stage, commit, push, and create a final pull request when you are done.  Note that the last data and time stamp of the last pull request will be considered the submission time for late penalties.

---

## Parts 1, 2, 3

We have broken down this project into 3 parts, about 1 week's work each to help you stay on track.

**You will only turn in the project once  at the end of part 3!**

- In Part 1, we will query using the Google BigQuery GUI interface in the cloud.

- In Part 2, we will query using the Linux command line from our virtual machine in the cloud.

- In Part 3, we will query from a Jupyter Notebook in our virtual machine in the cloud, save the results into Pandas, and present a report enhanced by Pandas output tables and simple data visualizations using Seaborn / Matplotlib.

---

## Part 1 - Querying Data with BigQuery

### SQL Tutorial

Please go through this SQL tutorial to help you learn the basics of SQL to help you complete this project.

SQL tutorial: https://www.w3schools.com/sql/default.asp

### Google Cloud Helpful Links

Read: https://cloud.google.com/docs/overview/

BigQuery: https://cloud.google.com/bigquery/

Public Datasets: Bring up your Google BigQuery console, open the menu for the public datasets, and navigate to the the dataset san_francisco.

- The Bay Bike Share has two datasets: a static one and a dynamic one.  The static one covers an historic period of about 3 years.  The dynamic one updates every 10 minutes or so.  THE STATIC ONE IS THE ONE WE WILL USE IN CLASS AND IN THE PROJECT. The reason is that is much easier to learn SQL against a static target instead of a moving target.

- (USE THESE TABLES!) The static tables we will be using in this class are in the dataset **san_francisco** :

  * bikeshare_stations

  * bikeshare_status

  * bikeshare_trips

- The dynamic tables are found in the dataset **san_francisco_bikeshare**

### Some initial queries

Paste your SQL query and answer the question in a sentence.  Be sure you properly format your queries and results using markdown. 

- What's the size of this dataset? (i.e., how many trips)
```sql
SELECT COUNT(DISTINCT(trip_id)) AS trip_count
FROM `bike_trip_data.bikeshare_trips` 
```
There has been a total of 983,648 recorded trips.
- What is the earliest start date and time and latest end date and time for a trip?

```sql
SELECT MIN(start_date ) as min_start_date, MAX(end_date) as max_end_date
FROM `bike_trip_data.bikeshare_trips` 
```
The earliest start date and time was on August 29, 2013 at 9:08am PT. The latest end date and time was on August 31, 2016 at 11:48 PM PT.
- How many bikes are there?
```sql
SELECT COUNT(DISTINCT(bike_number))
FROM `bike_trip_data.bikeshare_trips`
```
There are a total of 700 bikes.


### Questions of your own
- Make up 3 questions and answer them using the Bay Area Bike Share Trips Data.  These questions MUST be different than any of the questions and queries you ran above.

- Question 1: At what three times of the day does the bike share service have the most users and does it coincide with commuting hours?
  * Answer: The three most popular hours of the day are 8am with a total of 132,464 rides, 5pm with a total of 126,302 rides, and 9am with 96,118 rides.
  * SQL query:
```sql
SELECT start_hour, COUNT(start_hour) as most_pop_hour
  FROM `bike_trip_data.time_of_trip` 
  GROUP BY start_hour 
  ORDER BY most_pop_hour DESC
```

- Question 2: What are the 3 most common trips that starts in one bike share station and ends in another?
  * Answer:The most common trip is from Harry Bridges Plaza to Embarcadero at Sansome, the second most common trip is from the San Francisco Caltrain to Townsend at 7th, and the third most popular trip is from 2nd at townsend to Harry Bridges Plaza. 
  * SQL query:
```sql
SELECT start_station_name, end_station_name, COUNT(start_station_name) as count_trip_a_b
FROM `bike_trip_data.bikeshare_trips` 
WHERE start_station_name != end_station_name 
GROUP BY start_station_name , end_station_name 
ORDER BY count_trip_a_b DESC
```

- Question 3:What is the most common rental legnth before adjusting for unrealistic travel times and what is the most common travel time after adjustment?
  * Answer:The most common travel length before adjustment is 16.99 minutes. On relatively flat ground a mile on a bike takes 4 minutes. I'll consider all rides greater than or equal to 4 minutes valid since roughly 1 mile is a distance where someone may consider biking. I filtered out rentals longer than nine hours out because they exceed a full workday. After filtering the data as described, I found an average of 14.951 minutes spent on the bike.
  * SQL query:
Non-filtered query
```sql
SELECT AVG(duration_minutes) AS average_time_on_bike, MAX(duration_minutes) AS max_time_on_bike, 
      MIN(duration_minutes) AS min_time_on_bike
   FROM `bike_trip_data.time_on_bike` 
```
Query to filter "invalid trips"
```sql
SELECT * 
FROM `bike_trip_data.time_on_bike` 
WHERE duration_minutes >= 4 AND duration_minutes <= 540
```
Query to find average of filtered trips
```sql
SELECT AVG(duration_minutes) as average_time_on_bike
FROM `bike_trip_data.filtered_time` 
```


### Bonus activity queries (optional - not graded - just this section is optional, all other sections are required)

The bike share dynamic dataset offers multiple tables that can be joined to learn more interesting facts about the bike share business across all regions. These advanced queries are designed to challenge you to explore the other tables, using only the available metadata to create views that give you a broader understanding of the overall volumes across the regions(each region has multiple stations)

We can create a temporary table or view against the dynamic dataset to join to our static dataset.

Here is some SQL to pull the region_id and station_id from the dynamic dataset.  You can save the results of this query to a temporary table or view.  You can then join the static tables to this table or view to find the region:
```sql
#standardSQL
select distinct region_id, station_id
from `bigquery-public-data.san_francisco_bikeshare.bikeshare_station_info`
```

- Top 5 popular station pairs in each region

- Top 3 most popular regions(stations belong within 1 region)

- Total trips for each short station name in each region

- What are the top 10 used bikes in each of the top 3 region. these bikes could be in need of more frequent maintenance.

---

## Part 2 - Querying data from the BigQuery CLI 

- Use BQ from the Linux command line:

  * General query structure

    ```
    bq query --use_legacy_sql=false '
        SELECT count(*)
        FROM
           `bigquery-public-data.san_francisco.bikeshare_trips`'
    ```

### Queries

1. Rerun the first 3 queries from Part 1 using bq command line tool (Paste your bq
   queries and results here, using properly formatted markdown):

  * What's the size of this dataset? (i.e., how many trips)
```sql
bq query --use_legacy_sql=false '
SELECT COUNT(DISTINCT(trip_id)) AS trip_count
 FROM `bike_trip_data.bikeshare_trips`'
```
There have been 983,648 trips.

  * What is the earliest start time and latest end time for a trip?
```sql
bq query --use_legacy_sql=false '
SELECT MIN(start_date ) as min_start_date, MAX(end_date) as max_end_date
FROM `bike_trip_data.bikeshare_trips`'
```
The earliest start time was August 29, 2013 at 9:08 am and the latest end time was August 31, 2016 at 11:48pm.

  * How many bikes are there?
```sql
bq query --use_legacy_sql=false '
SELECT COUNT(DISTINCT(bike_number))
FROM `bike_trip_data.bikeshare_trips`'
```
There are 700 bikes.

2. New Query (Run using bq and paste your SQL query and answer the question in a sentence, using properly formatted markdown):

  * How many trips are in the morning vs in the afternoon?
```sql 
bq query --use_legacy_sql=false '
SELECT
	COUNT(*) as Total_Count,
         SUM(CASE WHEN start_hour BETWEEN 6 and 12 THEN 1 ELSE NULL END) Total_Morning,
         SUM(CASE WHEN start_hour Between 12 AND 18 THEN 1 ELSE NULL END) Total_Afternoon  
 FROM    `bike_trip_data.time_of_trip` '
```
I considered the morning from 6am to 11:59am and the afternoon from 12:00 to 6:00pm. For the morning, there were a total of 446,771 trips and in the afternoon there were a total of 475,768 trips
### Project Questions
Identify the main questions you'll need to answer to make recommendations (list
below, add as many questions as you need).

- Question 1: How many bikes are in use on average by hour and day of the week?

- Question 2: What fraction of trips are taken by subsribers?

- Question 3: How many trips appear to be commuter trips per day on average?

- Question 4: How much time is spent on the bike for commuter trips?

- Question 5: Do areas with more hills have lower usage?

- Question 6: What days of the week is usage highest?

- Question 7: Are there days where there is more demand than there are bikes?

- Question 8: Are there more bikes then demand?


### Answers

Answer at least 4 of the questions you identified above You can use either
BigQuery or the bq command line tool.  Paste your questions, queries and
answers below.

- Question 1: How many bikes are in use on average per hour and day of the week? (from 6am-7pm)
  * Answer: 
Sunday: 6am: 1.06, 7am: 1.11, 8am: 1.18, 9am: 1.26, 10am:1.35, 11am:1.41, 12pm: 1.45, 1pm:1.44, 2pm:1.45,3pm: 1.45, 4pm: 1.44, 5pm:1.37, 6pm:1.32, 7pm:1.27
Monday:6am:1.33, 7am:2.09, 8am:3.16, 9am:2.41, 10am:1.48, 11am:1.389, 12pm:1.46, 1pm:1.41, 2pm: 1.36, 3pm: 1.48, 4pm: 2.16, 5pm: 3.00, 6pm:2.24, 7pm:1.57
Tuesday:6am 1.38, 7am: 2.17, 8am: 3.32, 9am:2.466, 10am:1.48, 11am:1.42, 12pm:1.511, 1pm:1.43, 2pm:1.34, 3pm: 1.50, 4pm: 2.20, 5pm: 2.99, 6pm: 2.31, 7pm:1.56
Wednesday:6am:1.34, 7am: 2.10, 8am:3.25, 9am:2.50, 10am:1.49, 11am:1.40, 12pm:1.50, 1pm:1.45, 2pm:1.35, 3pm:1.51, 4pm:2.16, 5pm:2.94, 6pm:2.27, 7pm:1.58
Thursday: 6am:1.32, 7am:2.08, 8am:3.20, 9am: 2.45, 10am:1.51, 11am:1.46, 12pm:1.53, 1pm:1.48, 2pm:1.40, 3pm: 1.56, 4pm:2.15, 5pm:2.86, 6pm:2.20, 7pm:1.55
Friday: 6am: 1.30, 7am:1.94, 8am: 2.86, 9am:2.34, 10am:1.50, 11am:1.48, 12pm:1.53, 1pm:1.52, 2pm:1.46, 3pm:1.59, 4pm:2.13, 5pm:2.56, 6pm:1.91, 7pm: 1.43
Saturday: 6am:1.06, 7am: 1.08, 8am:1.17, 9am:1.26, 10am:1.36, 11am:1.45, 12pm:1.49, 1pm:1.51, 2pm:1.48, 3pm: 1.46, 4pm: 1.48, 5pm:1.37, 6pm:1.36, 7pm 1.28   
  * SQL query:
```sql
select
dow, start_hour, avg(number_trips)
from
(
SELECT
       start_date,
       EXTRACT(DAYOFWEEK FROM start_date) as dow,
       EXTRACT(HOUR FROM start_date) AS start_hour,
       count(*) as number_trips
FROM `bigquery-public-data.san_francisco.bikeshare_trips`
group by start_date, dow, start_hour
)
group by dow, start_hour
order by 1, 2
```

- Question 2:What fraction of percent are taken by subscribers?
  * Answer:There are substantially more subscribers than customers. The trips are taken by 86% subscribers.
  * SQL query:
```sql
SELECT
  SUM(CASE subscriber_type WHEN 'Customer' THEN 1 ELSE NULL END)/COUNT(*) AS Customer
  ,SUM(CASE subscriber_type WHEN 'Subscriber' THEN 1 ELSE NULL END)/COUNT(*) as Subscriber
FROM `bike_trip_data.bikeshare_trips` 
```

- Question 3:How many trips appear to be commuter trips per day on average?
 * Answer: I decided that trips between 6am and 10am and 4pm and 8pm are mostly commuter trips. I also filtered the data by not including trips that did not go to two unique stations. Moreover, I did not include trips on Saturdays or Sundays and trips that had a duration less than 6 minutes. I found that there are only 2.10 commuter trips on average on Mondays, 2.15 on Tuesdays, 2.13 on Wednesdays, 2.10 on Thursdays, and 1.98 on Fridays.
 * SQl query:
 ```sql
 SELECT dow, AVG(number_trips) as average_trips
FROM(
    SELECT
       start_date,
       EXTRACT(DAYOFWEEK FROM start_date) as dow, 
       count(*) as number_trips
  FROM `bigquery-public-data.san_francisco.bikeshare_trips`
  WHERE 
          (EXTRACT(HOUR FROM start_date) > 5 AND EXTRACT(HOUR FROM start_date) <= 10) OR (EXTRACT(HOUR FROM start_date) >= 16 AND EXTRACT(HOUR FROM start_date) <= 20)
          AND start_station_name != end_station_name AND duration_sec > 360
  GROUP BY start_date, dow
)
GROUP BY dow
HAVING dow!= 1 AND dow!=7
ORDER BY dow 
```


- Question 4: How much time is spent on a bike on the average commuter trip by day of the week?
 * Answer: On Monday on average a commuter spends 26 minutes on the bike, almost 26 minutes on Tuesday, 26 minutes on Wednesday, 26 minutes on Thursday, and almost 28 minutes on Friday
 * SQL query:
 ```sql
 SELECT dow, AVG(duration_bike)/60
FROM(
    SELECT
       start_date,
       EXTRACT(DAYOFWEEK FROM start_date) as dow,
       SUM(duration_sec) as duration_bike
  FROM `bigquery-public-data.san_francisco.bikeshare_trips`
  WHERE 
          (EXTRACT(HOUR FROM start_date) > 5 AND EXTRACT(HOUR FROM start_date) <= 10) OR (EXTRACT(HOUR FROM start_date) >= 16 AND EXTRACT(HOUR FROM start_date) <= 20)
          AND start_station_name != end_station_name AND duration_sec > 360
  GROUP BY start_date, dow
)
GROUP BY dow
HAVING dow!= 1 AND dow!=7
ORDER BY dow 
```

```python
my_panda_data_frame
```

#### Report in the form of the Jupter Notebook named Project_1.ipynb

- Using markdown cells, MUST definitively state and answer the two project questions:

  * What are the 5 most popular trips that you would call "commuter trips"? 
  
  * What are your recommendations for offers (justify based on your findings)?

- For any temporary tables (or views) that you created, include the SQL in markdown cells

- Use code cells for SQL you ran to load into Pandas, either using the !bq or the magic commands

- Use code cells to create Pandas formatted output tables (at least 3) to present or support your findings

- Use code cells to create simple data visualizations using Seaborn / Matplotlib (at least 2) to present or support your findings

### Resource: see example .ipynb file 

[Example Notebook](example.ipynb)

