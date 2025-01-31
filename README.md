README
================

## About the project

The data and R script included in this repository were used in the
analysis for the research article by Wegner et al. (submitted to Journal
of Dairy Science) titled “Suckling behavior of dairy calves in two types
of dam-rearing systems”. For details on study design, please refer to
the materials and methods as written in the article.

## Description of variables

For further details on how each variable was collected and/or
calculated, please refer to the full article.

### calf_id

The ID of the focal calf.

### breed

The breed of the calf: Swedish Holstein (sh) or Swedish Red (srb).

### heifer_calf

The sex of the calf, where 0 indicates a bull calf and 1 indicates a
heifer calf.

### birth_bw

The body weight of the calf as weighed immediately after birth, in kg.

### study

Which individual study the focal event belongs to. Data was collected
from calves housed with either cow-driven or calf-driven contact with
dams, systems which were run as separate experimental studies.

### cow_id

The ID of the focal cow.

### dam

Whether or not the focal cow was also the dam to the focal calf; 1 =
yes, 0 = no.

### dam_present

Whether the dam was present in the contact area, and thus physically
available to the calf, at the beginning of the focal event. 1 = yes, 0 =
no. If the variable ‘dam’ was scored a 1, then dam_present was
automatically also scored a 1.

### position

The body position of the calf during suckling bouts, which was based on
angle of the calf’s body relative to the body of the cow. Possible
positions included reverse parallel (R), from the side (S) or from
behind (B). Only applicable to suckling bouts.

### other_calves

Whether or not (1 = yes, 0 = no) other calves were engaged in suckling
on the focal cow at the start of a focal event. In order to be scored a
1, the non-focal calf had to have a confirmed suckling bout of their
own, and physically be in contact with the udder at the start time of
the focal behavioral event.

### termination_reason

The reason for the termination of the focal behavioral event. The
category ‘other’ includes disruptions by non-focal animals, personnel or
barn equipment (e.g., barn scrapers).

### event_length_s

The total duration of the focal behavioral event in seconds.

### behavior

The type of behavior, with definitions as follows:

Suckling bout - The calf is near (\<10 cm) or touching the udder with
its mouth for ≥1 min and visibly, rhythmically sucking throughout.
Contact between the mouth and udder can be broken for periods of \<1
min. Suckling bouts that occur within 10 minutes are considered part of
the same event.

Close-to-udder event - The calf is near (\<10 cm) or touching the udder
with its mouth, but with \<1 min or no visible sucking activity. Close
contacts that occur \<1 min apart are considered a single close-to-udder
event.

### calf_age

The average calf age in weeks; included in all analyses as a continuous,
numerical variable.

### min_to_next_SB

The time in minutes from the end of a close-to-udder event to the start
of the next, closest suckling bout for the same calf, on the same
observation day. Only applicable to close-to-udder events.

### allosuckling

Whether or not a suckling bout occured on the dam (1) or an alien cow
(0). Only applicable to suckling bouts.

### twin

Whether or not the calf was a twin. 1 = yes, 0 = no.

### allogrooming

Defined as licking between a focal cow and calf during, or within 1 min
before or after, a suckling bout. Can be directed to any part of the
recipient’s body.
