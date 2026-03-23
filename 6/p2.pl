% plane(plane_num, model, age).
% flight(flight_num, from, to, plane_num).
% passenger(sin, name, city, age).
% booked(sin, flight_num, date, price).

plane(p123, boeing747, 8).
plane(p234, boeing747, 12).
plane(p345, airbus320, 5).
plane(p456, boeing777, 20).
plane(p567, airbus380, 3).
plane(p678, boeing747, 15).

flight(f407, toronto, paris, p123).
flight(f237, losangeles, paris, p234).
flight(f345, paris, newyork, p345).
flight(f456, toronto, newyork, p456).
flight(f567, losangeles, newyork, p567).
flight(f678, newyork, paris, p678).
flight(f789, newyork, paris, p456).

passenger(s456_234_987, anna, toronto, 26).
passenger(s567_345_678, bob, losangeles, 35).
passenger(s678_456_789, carol, paris, 42).
passenger(s789_567_890, dave, newyork, 30).
passenger(s890_678_901, eve, toronto, 22).
passenger(s901_789_012, frank, losangeles, 29).

booked(s456_234_987, f407, jan17, 500).
booked(s456_234_987, f237, jan17, 700).
booked(s567_345_678, f237, jan17, 650).
booked(s901_789_012, f237, jan17, 800).
booked(s678_456_789, f345, feb23, 600).
booked(s789_567_890, f456, mar10, 400).
booked(s890_678_901, f567, jan17, 300).
booked(s567_345_678, f237, feb23, 720).
booked(s890_678_901, f237, feb23, 680).

% The model and age of plane p123.
%?- plane(p123, Model, Age).

% The plane number and age of every boeing 747.
%?- plane(PlaneNum, boeing747, Age).

% The flight number of every plane going from New York to Paris.
%?- flight(FlightNum, newyork, paris, PlaneNum).

% The flight number and price paid by every passenger going from Los Angeles to Paris on Jan 17.
%?- booked(SIN, FlightNum, jan17, Price), flight(FlightNum, losangeles, paris, PlaneNum).

% The name and SIN of every passenger booked on flight f237 on Feb 23.
%?- booked(SIN, f237, feb23, Price), passenger(SIN, Name, City, Age).