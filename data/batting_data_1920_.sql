create table bat_p as
select batting.playerID,
batting.yearID,
people.nameFirst,
People.nameLast,
(batting.yearID - people.birthYear) as age,
people.weight,
"Batting"."teamID", "Batting"."lgID", "Batting"."G", "Batting"."AB", "Batting"."R", "Batting"."H", "Batting"."2B", "Batting"."3B", "Batting"."HR", "Batting"."RBI", "Batting"."SB", "Batting"."CS", "Batting"."BB", "Batting"."SO", "Batting"."IBB", "Batting"."HBP", "Batting"."SH", "Batting"."SF",
people.birthYear,
people.birthMonth,
people.bats,
(people.finalGame-people.debut) as num_years

from batting
inner join people on batting.playerID = people.playerID
where yearID > 1920 and num_years > 5
;
