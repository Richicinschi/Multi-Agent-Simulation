;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disease Spread Model v2.0
;; A spatially-explicit epidemiological simulation using real-world geographic data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required extensions for model functionality
extensions [gis table] ;profiler

;; Global variables used throughout the simulation
globals [
  countries-dataset        ; GIS dataset containing country boundaries and data
  country-temperatures    ; Table storing average temperatures for each country
  total-population       ; Total number of agents in the simulation
  initial-population     ; Initial number of agents (preserved for calculations)
  selected-country       ; Country selected for initial infection
  isolated-countries     ; List of countries currently under isolation

  ; Transport system counters
  airport-usage-count    ; Number of times airports have been used
  port-usage-count       ; Number of times ports have been used

  ; Disease tracking variables
  total-infected-previous-tick  ; Number of infected agents in previous tick
  new-infections-this-tick     ; Number of new infections in current tick
  highest-daily-increase       ; Highest number of daily infections recorded

  ; Statistical tracking
  most-affected-countries     ; Table tracking infection counts by country
  death-toll-by-country      ; Table tracking deaths by country
  most-infected-country      ; Country with highest current infection count
  most-deaths-country        ; Country with highest death count

  ; Interface and mode controls
  select-country-mode?      ; Whether user can select initial infection country
  turtles-just-traveled    ; Set of turtles that just used transportation

  ; Performance optimization caches
  temperature-cache         ; Cache for patch temperature calculations
  country-patches-cache     ; Cache mapping countries to their patches
  country-turtle-cache      ; Cache mapping countries to their turtles
  last-isolation-update     ; Tick counter for isolation status updates
]

;; Properties for each patch (geographic location)
patches-own [
  population-density  ; Population density from GIS data
  country-name       ; Name of country this patch belongs to
  is-airport?        ; Whether this patch is an airport
  is-port?           ; Whether this patch is a seaport
  temperature        ; Local temperature
  original-color     ; Original patch color (for restoring after isolation)
]

;; Properties for each agent (person)
turtles-own [
  status           ; Current health status (healthy/infected/recovered/dead)
  days-infected    ; Number of days since infection
  travel-cooldown  ; Cooldown period after using transport
  recovery-time    ; Individual time to recover from infection
  compliant?       ; Whether agent complies with isolation measures
  home-country     ; Agent's home country
  destination      ; Current movement target
  cached-temp      ; Cached local temperature for performance
  days-recovered   ; Days since recovery began
]

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETUP PROCEDURES
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Main setup procedure - initializes the entire simulation
to setup
  ;profiler:start       ; Begin performance profiling

  clear-all           ; Reset the simulation
  reset-ticks        ; Reset tick counter
  init-globals       ; Initialize global variables
  setup-map         ; Set up the geographic environment
  setup-countries   ; Configure country properties
  setup-caches      ; Initialize optimization caches
  create-agents     ; Create the agent population

  ; Store initial population for percentage calculations
  set initial-population count turtles

  setup-plotss      ; Initialize graphs and plots

  ; Handle initial infection based on user preference
  ifelse select-infection-country? [
    prompt-for-country
  ] [
    infect-initial-agents
  ]

  ; End setup profiling
  ;profiler:stop
  ;print profiler:report
end

;; Initialize all global variables to their starting values
to init-globals
  ; Reset interface control variables
  set select-country-mode? false
  set selected-country ""

  ; Reset transport counters
  set airport-usage-count 0
  set port-usage-count 0

  ; Reset disease tracking variables
  set total-infected-previous-tick 0
  set new-infections-this-tick 0
  set highest-daily-increase 0
  set turtles-just-traveled no-turtles

  ; Initialize statistical tracking tables
  set most-affected-countries table:make
  set death-toll-by-country table:make
  set temperature-cache table:make
  set country-patches-cache table:make
  set isolated-countries []

  ; Initialize tracking variables
  set most-infected-country ""
  set most-deaths-country ""

  ; Initialize optimization caches
  set country-turtle-cache table:make
  set last-isolation-update 0
end

;; Set up the geographic environment using GIS data
to setup-map
  ; Load and configure the world map
  set countries-dataset gis:load-dataset "ne_110m_admin_0_countries.shp"
  gis:set-world-envelope gis:envelope-of countries-dataset

  ; Initialize all patches with default values
  ask patches [
    set pcolor blue             ; Water by default
    set original-color blue
    set population-density 0
    set country-name ""
    set is-airport? false
    set is-port? false
    set temperature 0
  ]

  ; Apply GIS data to patches
  gis:apply-coverage countries-dataset "POP_EST" population-density
  gis:apply-coverage countries-dataset "NAME" country-name

  ; Handle patches without valid country names
  ask patches [
    if not is-string? country-name or country-name = "" [
      set country-name "Unknown"
    ]
  ]
end

;; Configure countries and their properties
to setup-countries
  ; Initialize temperature tracking
  set country-temperatures table:make
  load-country-temperatures "temp.csv"

  ; Process and cache temperatures for all valid patches
  ask patches with [country-name != "Unknown"] [
    ; Set temperature based on country data
    ifelse table:has-key? country-temperatures country-name [
      set temperature table:get country-temperatures country-name
    ] [
      set temperature 0
    ]

    ; Cache temperature for performance
    let patch-id (word pxcor "," pycor)
    table:put temperature-cache patch-id temperature
  ]

  ; Set patch colors based on temperature and population
  ask patches with [population-density > 0 and country-name != "Antarctica"] [
    ifelse temperature > heat-threshold [
      set pcolor orange
      set original-color orange
    ] [
      ifelse temperature < cold-threshold [
        set pcolor grey
        set original-color grey
      ] [
        set pcolor green
        set original-color green
      ]
    ]
  ]

  ; Draw country boundaries
  gis:draw countries-dataset black

  ; Set up transportation infrastructure if enabled
  if airports-and-ports-enabled? [
    assign-airports-and-ports
  ]
end

;; Initialize performance optimization caches
to setup-caches
  ; Get list of valid countries for processing
  let valid-countries remove-duplicates [country-name] of patches with [
    country-name != "Unknown" and
    country-name != "Antarctica" and
    country-name != ""
  ]

  ; Cache patches for each country for faster access
  foreach valid-countries [ country ->
    let country-patches patches with [country-name = country]
    table:put country-patches-cache country country-patches
  ]
end

;; Load temperature data for each country from CSV file
to load-country-temperatures [filename]
  file-open filename
  while [not file-at-end?] [
    let line file-read-line
    let delimiter ","
    let delimiter-pos position delimiter line

    ; Parse country and temperature data
    if delimiter-pos != false [
      let country substring line 0 delimiter-pos
      let temp substring line (delimiter-pos + 1) length line
      let temp-number read-from-string temp
      table:put country-temperatures country temp-number
    ]
  ]
  file-close
end

;; Set up transportation infrastructure (airports and ports)
to assign-airports-and-ports
  ; Clear any existing transportation infrastructure
  ask patches [
    set is-airport? false
    set is-port? false
  ]

  ; Define criteria for suitable airport locations
  let suitable-airport-patches patches with [
    country-name != "" and
    country-name != "Unknown" and
    country-name != "Antarctica" and
    pcolor != blue and
    not any? other patches in-radius 4 with [is-airport?]
  ]

  ; Place airports based on user-defined number
  let airports-to-place num-airports
  repeat airports-to-place [
    if any? suitable-airport-patches [
      ask one-of suitable-airport-patches [
        set is-airport? true
        set pcolor yellow
        ; Create airport visual area
        ask patches in-radius 1 [
          set pcolor yellow
        ]
      ]
      ; Update available patches for next airport
      set suitable-airport-patches patches with [
        country-name != "" and
        country-name != "Unknown" and
        country-name != "Antarctica" and
        pcolor != blue and
        pcolor != yellow and
        not any? other patches in-radius 4 with [is-airport?]
      ]
    ]
  ]

  ; Define criteria for suitable port locations
  let suitable-port-patches patches with [
    country-name != "" and
    country-name != "Unknown" and
    country-name != "Antarctica" and
    not is-airport? and
    pcolor != blue and
    any? neighbors with [pcolor = blue] and
    not any? other patches in-radius 4 with [is-port?]
  ]

  ; Place ports based on user-defined number
  let ports-to-place num-ports
  repeat ports-to-place [
    if any? suitable-port-patches [
      ask one-of suitable-port-patches [
        set is-port? true
        set pcolor brown
        ; Create port visual area
        ask patches in-radius 1 [
          set pcolor brown
        ]
      ]
      ; Update available patches for next port
      set suitable-port-patches patches with [
        country-name != "" and
        country-name != "Unknown" and
        country-name != "Antarctica" and
        not is-airport? and
        pcolor != blue and
        pcolor != brown and
        any? neighbors with [pcolor = blue] and
        not any? other patches in-radius 4 with [is-port?]
      ]
    ]
  ]
end

;; Create and distribute agents across the world based on population density
to create-agents
  ; Calculate base number of agents per population unit
  let base-scaling-factor 0.000005

  ; Get list of valid countries for agent placement
  let valid-countries remove-duplicates [country-name] of patches with [
    country-name != "Unknown" and
    country-name != "Antarctica" and
    country-name != ""
  ]

  ; Create agents for each country based on population
  foreach valid-countries [ country ->
    let country-patches patches with [country-name = country]
    let country-population [population-density] of one-of country-patches

    ; Only create agents if country has population
    if country-population > 0 [
      ; Calculate number of agents based on population and scaling
      let num-agents floor (country-population * base-scaling-factor * population-scaling)
      if num-agents < 1 and population-scaling > 0 [ set num-agents 1 ]

      ; Create agents with initial properties
      create-turtles num-agents [
        ; Place agent in valid location within country
        move-to one-of country-patches with [pcolor != blue]

        ; Set initial agent properties
        set status "healthy"
        set days-recovered 0
        set shape "person"
        set color white
        set size 1.5
        set compliant? false
        set destination nobody
        set travel-cooldown 0

        ; Set country-specific properties
        set home-country [country-name] of patch-here
        set cached-temp [temperature] of patch-here
      ]
    ]
  ]

  ; Store total population for statistics
  set total-population count turtles
end

;; Handle the initial infection setup based on user selection
to prompt-for-country
  let valid-country? false
  while [not valid-country?] [
    ; Get user input for starting country
    let country-input user-input "Enter the name of the country to start infection (e.g., United States of America)"

    ; Validate country selection
    ifelse any? patches with [country-name = country-input and pcolor != blue] [
      set selected-country country-input
      set valid-country? true
      user-message (word "Selected country for infection: " selected-country)
      infect-initial-agents-in-country selected-country
    ] [
      user-message (word "Invalid country name: '" country-input "'. Please enter the exact country name.")
    ]
  ]
end

;; Start infection in a specific country
to infect-initial-agents-in-country [country]
  ; Find eligible agents in the selected country
  let candidates turtles with [
    status = "healthy" and
    [country-name] of patch-here = country
  ]
  ; Calculate number of agents to infect
  let infect-count min list initial-infected count candidates

  ; Infect agents if possible
  ifelse infect-count > 0 [
    ask n-of infect-count candidates [ become-infected ]
    user-message (word infect-count " agents infected in " country)
  ] [
    user-message (word "No healthy agents available in " country)
  ]
end

;; Start infection randomly across the world
to infect-initial-agents
  let candidates turtles with [status = "healthy"]
  let infect-count min list initial-infected count candidates
  ask n-of infect-count candidates [ become-infected ]
end

;; Main simulation step procedure
to go
  ; Start performance profiling
  ;profiler:reset
  ;profiler:start

  ; Check if simulation should continue
  if not any? turtles with [status = "infected"] [ stop ]

  ; Execute main simulation steps
  update-isolation-status
  move-turtles
  spread-disease
  update-status
  update-plotss
  update-statistics
  export-statistics
  tick

  ; End profiling and report results
  ;profiler:stop
  ;print profiler:report
end

;; Manages the spread of disease between agents
to spread-disease
  ; Identify all infected agents
  let infected-turtles turtles with [status = "infected"]

  ; Process each infected agent
  ask infected-turtles [
    let adjusted-chance infection-chance

    ; Adjust infection chance based on temperature
    if cached-temp > heat-threshold [
      set adjusted-chance adjusted-chance * virus-heat-resistance
    ]
    if cached-temp < cold-threshold [
      set adjusted-chance adjusted-chance * virus-cold-resistance
    ]

    ; Attempt to infect nearby healthy agents
    let potential-targets turtles in-radius 1 with [status = "healthy"]
    if any? potential-targets [
      ask potential-targets [
        if random-float 1 < adjusted-chance [ become-infected ]
      ]
    ]
  ]
end

;; Handle the infection process for a single agent
to become-infected
  set status "infected"
  set color red
  set days-infected 0
  ; Calculate recovery time with normal distribution
  set recovery-time random-normal recovery-time-mean recovery-time-sd
  if recovery-time < min-recovery-time [ set recovery-time min-recovery-time ]
  set new-infections-this-tick new-infections-this-tick + 1
end

;; Update health status of all agents
to update-status
  ; Process infected agents
  ask turtles with [status = "infected"] [
    set days-infected days-infected + 1
    if days-infected >= recovery-time [
      ; Determine if agent dies or recovers
      ifelse random-float 1 < mortality-rate [
        set status "dead"
        set color black
      ] [
        set status "recovered"
        set color yellow
        set days-recovered 0
      ]
    ]
  ]

  ; Process recovered agents
  ask turtles with [status = "recovered"] [
    set days-recovered days-recovered + 1
    if days-recovered >= days-until-healthy [
      set status "healthy"
      set color white
      set days-recovered 0
    ]
  ]
end

;; Controls movement and transportation of all agents
to move-turtles
  ; Identify all agents that can move (not dead)
  let mobile-turtles turtles with [status != "dead"]

  ; Create a pre-filtered set of agents in isolated countries for performance
  let isolated-set nobody
  if isolation-enabled? [
    set isolated-set turtle-set mobile-turtles with [member? home-country isolated-countries]
  ]

  ; Process movement for each mobile agent
  ask mobile-turtles [
    let should-move? true

    ; Check if agent should comply with isolation measures
    if isolation-enabled? and member? self isolated-set [
      if random-float 1 < compliance-rate [
        set should-move? false
      ]
    ]

    ; Execute movement if allowed
    if should-move? [
      ; Set movement speed based on health status
      let step-size 0.5
      if status = "infected" [ set step-size 0.25 ]

      ; Store current location information
      let current-country [country-name] of patch-here

      ; Handle transportation system usage
      if airports-and-ports-enabled? and travel-cooldown = 0 [
        ; Check for nearby transportation infrastructure
        let nearby-transport patches in-radius 2 with [is-airport? or is-port?]
        if any? nearby-transport and random-float 1 < airport-travel-chance [
          ; Use appropriate transportation type
          ifelse any? nearby-transport with [is-airport?]
            [ use-airport ]
            [ use-port ]
          stop
        ]
      ]

      ; Regular movement logic
      ifelse destination = nobody [
        ; Occasionally select a new destination (20% chance per tick)
        if random-float 1 < 0.2 [
          ; Find valid movement locations within radius
          let valid-patches patches in-radius movement-radius with [
            country-name = current-country and
            pcolor != blue and
            not is-airport? and
            not is-port?
          ]

          ; Set new destination if valid patches found
          if any? valid-patches [
            set destination one-of valid-patches
            face destination
            ; Add slight randomness to movement direction
            rt random 20 - 10
          ]
        ]
      ] [
        ; Move towards existing destination
        ifelse distance destination < step-size [
          ; Arrived at destination
          move-to destination
          set destination nobody
        ] [
          ; Check if path to destination is valid
          let next-patch patch-ahead step-size
          ifelse next-patch != nobody and [country-name] of next-patch = current-country [
            fd step-size
          ] [
            ; Reset destination if path is blocked
            set destination nobody
          ]
        ]
      ]
    ]
  ]

  ; Update travel cooldowns in batch
  ask turtles with [travel-cooldown > 0] [
    set travel-cooldown travel-cooldown - 1
  ]
end

;; Handle air travel between countries
to use-airport
  let current-country [country-name] of patch-here

  ; Find all valid airport destinations
  let target-airports patches with [
    is-airport? and
    not member? country-name isolated-countries and
    country-name != current-country and
    country-name != "Unknown" and
    country-name != "Antarctica"
  ]

  ; Process airport travel if destinations available
  if any? target-airports [
    let chosen-airport one-of target-airports

    ; Find valid landing area in destination country
    let landing-area patches with [
      country-name = [country-name] of chosen-airport and
      distance chosen-airport <= 3 and
      pcolor != blue and
      not is-airport? and
      not is-port?
    ]

    ; Complete the travel if landing area found
    if any? landing-area [
      move-to one-of landing-area
      set airport-usage-count airport-usage-count + 1
      set travel-cooldown 10
    ]
  ]
end

;; Handle sea travel between countries
to use-port
  let current-country [country-name] of patch-here

  ; Find all valid port destinations
  let target-ports patches with [
    is-port? and
    not member? country-name isolated-countries and
    country-name != current-country and
    country-name != "Unknown" and
    country-name != "Antarctica"
  ]

  ; Process port travel if destinations available
  if any? target-ports [
    let chosen-port one-of target-ports

    ; Find valid landing area in destination country
    let landing-area patches with [
      country-name = [country-name] of chosen-port and
      distance chosen-port <= 3 and
      pcolor != blue and
      not is-port? and
      not is-airport?
    ]

    ; Complete the travel if landing area found
    if any? landing-area [
      move-to one-of landing-area
      set port-usage-count port-usage-count + 1
      set travel-cooldown 10
    ]
  ]
end

;; Manage country isolation based on infection rates
to update-isolation-status
  let old-isolated-countries isolated-countries
  set isolated-countries []

  ; Update turtle cache periodically for performance
  if ticks - last-isolation-update >= 10 [
    table:clear country-turtle-cache
    ask turtles [
      let c home-country
      ifelse table:has-key? country-turtle-cache c [
        ; Add to existing turtle-set for country
        let current-set table:get country-turtle-cache c
        table:put country-turtle-cache c (turtle-set current-set self)
      ] [
        ; Create new turtle-set for country
        table:put country-turtle-cache c (turtle-set self)
      ]
    ]
    set last-isolation-update ticks
  ]

  ; Process countries with active populations
  let active-countries table:keys country-turtle-cache

  foreach active-countries [ country ->
    if table:has-key? country-turtle-cache country [
      let country-turtles table:get country-turtle-cache country
      if any? country-turtles [
        ; Calculate infection rate for country
        let country-pop count country-turtles
        let infected-count count country-turtles with [status = "infected"]
        let country-infection-rate infected-count / country-pop

        ; Apply isolation if threshold exceeded
        if isolation-enabled? and country-infection-rate > infections-threshold [
          set isolated-countries lput country isolated-countries

          ; Update visualization only if status changed
          if not member? country old-isolated-countries and table:has-key? country-patches-cache country [
            let patches-to-update table:get country-patches-cache country
            if patches-to-update != nobody [
              ask patches-to-update [
                set pcolor violet
              ]
            ]
          ]
        ]
      ]
    ]
  ]

  ; Restore colors for countries no longer isolated
  foreach old-isolated-countries [ country ->
    if not member? country isolated-countries and table:has-key? country-patches-cache country [
      let patches-to-restore table:get country-patches-cache country
      if patches-to-restore != nobody [
        ask patches-to-restore with [pcolor = violet] [
          set pcolor original-color
        ]
      ]
    ]
  ]
end

;; Update all simulation statistics
to update-statistics
  ; Update highest daily increase record if necessary
  if new-infections-this-tick > highest-daily-increase [
    set highest-daily-increase new-infections-this-tick
  ]

  ; Update country-specific statistics
  update-country-statistics

  ; Store current infection count for next tick comparison
  set total-infected-previous-tick count turtles with [status = "infected"]
  set new-infections-this-tick 0
end

;; Calculate and update statistics for each country
to update-country-statistics
  ; Pre-filter infected and dead turtles for efficiency
  let infected-turtles turtles with [status = "infected"]
  let dead-turtles turtles with [status = "dead"]

  ; Initialize statistics tracking table
  let country-stats table:make

  ; Count infected agents by country in one pass
  ask infected-turtles [
    let c home-country
    let entry table:get-or-default country-stats c [0 0]
    table:put country-stats c replace-item 0 entry ((item 0 entry) + 1)
  ]

  ; Count dead agents by country in one pass
  ask dead-turtles [
    let c home-country
    let entry table:get-or-default country-stats c [0 0]
    table:put country-stats c replace-item 1 entry ((item 1 entry) + 1)
  ]

  ; Track maximum values for identifying most affected countries
  let max-infected-count 0
  let max-deaths-count 0

  ; Update statistics tables and find most affected countries
  foreach table:keys country-stats [ country ->
    let stats table:get country-stats country
    let infected-count item 0 stats
    let deaths-count item 1 stats

    ; Update country statistics tables
    table:put most-affected-countries country infected-count
    table:put death-toll-by-country country deaths-count

    ; Update most affected country if necessary
    if infected-count > max-infected-count [
      set max-infected-count infected-count
      set most-infected-country country
    ]

    ; Update country with most deaths if necessary
    if deaths-count > max-deaths-count [
      set max-deaths-count deaths-count
      set most-deaths-country country
    ]
  ]
end

;; Initialize all plots and graphs
to setup-plotss
  ; Set up daily new cases plot
  set-current-plot "Daily New Cases"
  clear-plot
  create-temporary-plot-pen "cases"
  set-plot-pen-color red

  ; Set up infection rate plot
  set-current-plot "Infection Rate"
  clear-plot
  create-temporary-plot-pen "rate"
  set-plot-pen-color red
end

;; Update all plots with current data
to update-plotss
  ; Update daily new cases plot
  set-current-plot "Daily New Cases"
  set-current-plot-pen "cases"
  plot new-infections-this-tick

  ; Update infection rate plot
  set-current-plot "Infection Rate"
  set-current-plot-pen "rate"
  plot (total-infected / initial-population) * 100
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPORTER PROCEDURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Report total number of currently infected agents
to-report total-infected
  report count turtles with [status = "infected"]
end

;; Report total number of recovered agents
to-report total-recovered
  report count turtles with [status = "recovered"]
end

;; Report total number of dead agents
to-report total-dead
  report count turtles with [status = "dead"]
end

;; Report total number of healthy agents
to-report total-healthy
  report count turtles with [status = "healthy"]
end

;; Calculate current infection rate as percentage
to-report infection-rate
  report (total-infected / initial-population) * 100
end

;; Calculate percentage of recovered agents
to-report recovered-percentage
  report precision ((total-recovered / initial-population) * 100) 1
end

;; Calculate percentage of healthy agents
to-report healthy-percentage
  report precision ((total-healthy / initial-population) * 100) 1
end

;; Calculate percentage of infected agents
to-report infected-percentage
  report precision ((total-infected / initial-population) * 100) 1
end

;; Calculate percentage of dead agents
to-report dead-percentage
  report precision ((total-dead / initial-population) * 100) 1
end

;; Verify total percentages add up correctly
to-report total-percentage
  report precision (
    (total-recovered + total-healthy + total-infected + total-dead)
    / initial-population * 100) 1
end

;; Determine which temperature region is most affected
to-report most-affected-region
  ; Count infections in different temperature regions
  let hot-regions count turtles with [
    status = "infected" and
    cached-temp > heat-threshold
  ]
  let cold-regions count turtles with [
    status = "infected" and
    cached-temp < cold-threshold
  ]
  let moderate-regions count turtles with [
    status = "infected" and
    cached-temp <= heat-threshold and
    cached-temp >= cold-threshold
  ]

  ; Find region with highest infection count
  let max-infected max (list hot-regions cold-regions moderate-regions)

  ; Return appropriate region name
  report ifelse-value (max-infected = hot-regions)
    [ "Hot Regions" ]
    [ ifelse-value (max-infected = cold-regions)
      [ "Cold Regions" ]
      [ "Moderate Regions" ]
    ]
end



to export-statistics
  ifelse ticks = 0 [
    ; Create new file with headers at start
    carefully [ file-delete "simulation_data.csv" ][]
    file-open "simulation_data.csv"
    file-type (word
      "tick,"
      ; General statistics
      "total_infected,total_recovered,total_dead,total_healthy,"
      ; Climate zone statistics
      "hot_region_infected,cold_region_infected,moderate_region_infected,"
      ; Transportation statistics
      "airport_usage,port_usage,"
      ; Isolation statistics
      "num_isolated_countries,"
      ; Regional spread
      "num_affected_countries,most_affected_country,most_deaths_country,"
      ; Temperature resistance impact
      "hot_region_transmission_rate,cold_region_transmission_rate,moderate_region_transmission_rate,"
      ; Movement patterns
      "cross_border_infections,within_country_infections"
      "\n")
  ][
    ; Append data for this tick
    file-open "simulation_data.csv"
  ]

  ; Calculate statistics for this tick
  let hot_infected count turtles with [status = "infected" and cached-temp > heat-threshold]
  let cold_infected count turtles with [status = "infected" and cached-temp < cold-threshold]
  let moderate_infected count turtles with [status = "infected" and
    cached-temp <= heat-threshold and cached-temp >= cold-threshold]

  ; Calculate transmission rates per region
  let hot_transmission calculate-transmission-rate heat-threshold "hot"
  let cold_transmission calculate-transmission-rate cold-threshold "cold"
  let moderate_transmission calculate-transmission-rate 0 "moderate"

  ; Count affected countries
  let affected_countries length remove-duplicates [country-name] of turtles with [status = "infected"]

  ; Write data row
  file-type (word
    ticks ","
    total-infected "," total-recovered "," total-dead "," total-healthy ","
    hot_infected "," cold_infected "," moderate_infected ","
    airport-usage-count "," port-usage-count ","
    length isolated-countries ","
    affected_countries "," most-infected-country "," most-deaths-country ","
    hot_transmission "," cold_transmission "," moderate_transmission ","
    count-cross-border-infections "," count-within-country-infections
    "\n")

  file-close
end

; Helper reporter to calculate transmission rate for a given region type
to-report calculate-transmission-rate [temp-threshold region-type]
  let region-turtles nobody
  if region-type = "hot" [
    set region-turtles turtles with [cached-temp > temp-threshold]
  ]
  if region-type = "cold" [
    set region-turtles turtles with [cached-temp < temp-threshold]
  ]
  if region-type = "moderate" [
    set region-turtles turtles with [
      cached-temp <= heat-threshold and
      cached-temp >= cold-threshold
    ]
  ]

  let total-in-region count region-turtles
  let infected-in-region count region-turtles with [status = "infected"]

  report ifelse-value (total-in-region > 0)
    [ infected-in-region / total-in-region ]
    [ 0 ]
end

; Helper reporter to count cross-border infections
to-report count-cross-border-infections
  report count turtles with [
    status = "infected" and
    travel-cooldown > 0  ; Recently traveled
  ]
end

; Helper reporter to count within-country-infections
to-report count-within-country-infections
  report count turtles with [
    status = "infected" and
    travel-cooldown = 0  ; Not recently traveled
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
465
10
1556
562
-1
-1
3.0
1
10
1
1
1
0
0
0
1
-180
180
-90
90
0
0
1
ticks
120.0

BUTTON
245
410
320
470
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
0
30
190
63
infection-chance
infection-chance
0
1
0.2
0.01
1
Probability
HORIZONTAL

BUTTON
331
410
406
470
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1275
510
1362
555
Total Healthy
count turtles with [status = \"healthy\"]
17
1
11

MONITOR
1365
510
1455
555
Total Infected
count turtles with [status = \"infected\"]
17
1
11

MONITOR
1180
510
1270
555
Total Recovered
count turtles with [status = \"recovered\"]
17
1
11

MONITOR
1460
510
1545
555
Total Dead
count turtles with [status = \"dead\"]
17
1
11

MONITOR
475
510
532
555
NIL
ticks
17
1
11

SLIDER
205
290
395
323
initial-infected
initial-infected
0
100
5.0
1
1
Agents
HORIZONTAL

SLIDER
205
250
395
283
population-scaling
population-scaling
0.01
1
0.3
0.01
1
Scale Factor
HORIZONTAL

SLIDER
205
70
400
103
num-airports
num-airports
0
100
24.0
1
1
Unit(s)
HORIZONTAL

SLIDER
205
110
400
143
num-ports
num-ports
0
100
17.0
1
1
Unit(s)
HORIZONTAL

BUTTON
246
482
406
527
Go Once (1 Tick)
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
0
70
190
103
mortality-rate
mortality-rate
0
1
0.1
0.01
1
Probability
HORIZONTAL

SLIDER
205
150
400
183
airport-travel-chance
airport-travel-chance
0
1
0.05
0.01
1
Proability
HORIZONTAL

SLIDER
205
190
400
223
port-travel-chance
port-travel-chance
0
1
0.03
0.01
1
Probability
HORIZONTAL

SLIDER
205
330
395
363
movement-radius
movement-radius
1
10
6.0
1
1
Patches
HORIZONTAL

SLIDER
0
230
190
263
recovery-time-mean
recovery-time-mean
1
150
30.0
1
1
Days/Ticks
HORIZONTAL

SLIDER
0
270
190
303
recovery-time-sd
recovery-time-sd
0
50
10.0
1
1
Days/Ticks
HORIZONTAL

SLIDER
0
190
190
223
min-recovery-time
min-recovery-time
1
100
20.0
1
1
Days/Ticks
HORIZONTAL

PLOT
465
570
980
790
Disease Progression
Ticks
Number of New Infections
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"recovered" 1.0 0 -7500403 true "" "plot total-recovered"
"infected" 1.0 0 -2674135 true "" "plot total-infected"
"dead" 1.0 0 -16777216 true "" "plot total-dead"
"healthy" 1.0 0 -1184463 true "" "plot total-healthy"

TEXTBOX
5
10
155
28
Disease Parameters
11
0.0
1

TEXTBOX
210
230
360
248
Population Parameters
11
0.0
1

TEXTBOX
205
10
355
28
Travel Parameters
11
0.0
1

SWITCH
205
30
400
63
airports-and-ports-enabled?
airports-and-ports-enabled?
0
1
-1000

SWITCH
0
490
200
523
select-infection-country?
select-infection-country?
0
1
-1000

TEXTBOX
5
465
155
483
Disease Control Strategies
11
0.0
1

SWITCH
0
530
200
563
isolation-enabled?
isolation-enabled?
1
1
-1000

SLIDER
0
610
200
643
compliance-rate
compliance-rate
0
1
0.5
0.1
1
Probability
HORIZONTAL

TEXTBOX
5
355
155
373
Climate Parameters\n
11
0.0
1

SLIDER
0
380
200
413
heat-threshold
heat-threshold
-30
50
13.0
1
1
°C
HORIZONTAL

SLIDER
0
420
200
453
cold-threshold
cold-threshold
-50
30
0.0
1
1
°C
HORIZONTAL

SLIDER
0
110
190
143
virus-heat-resistance
virus-heat-resistance
0
1
0.6
0.1
1
Fraction
HORIZONTAL

SLIDER
0
150
190
183
virus-cold-resistance
virus-cold-resistance
0
1
0.6
0.1
1
Fraction
HORIZONTAL

MONITOR
660
510
767
555
NIL
port-usage-count
17
1
11

MONITOR
535
510
657
555
NIL
airport-usage-count
17
1
11

SLIDER
0
570
200
603
isolation-compliance
isolation-compliance
0
1
0.3
0.1
1
Fraction
HORIZONTAL

PLOT
1565
285
1855
560
Daily New Cases
Ticks
Number of Cases
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"cases" 1.0 0 -7500403 true "" ""

MONITOR
600
460
680
505
Most Infected
most-infected-country
17
1
11

MONITOR
690
460
765
505
Most Deaths
most-deaths-country
17
1
11

MONITOR
475
460
590
505
Most Affected Region
most-affected-region
17
1
11

PLOT
1000
570
1345
790
Climate Impact
Ticks
Weather
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"hot" 1.0 0 -955883 true "" "plot count turtles with [status = \"infected\" and [temperature] of patch-here > heat-threshold]"
"cold" 1.0 0 -7500403 true "" "plot count turtles with [status = \"infected\" and [temperature] of patch-here < cold-threshold]"
"moderate" 1.0 0 -10899396 true "" "plot count turtles with [status = \"infected\" and [temperature] of patch-here <= heat-threshold and [temperature] of patch-here >= cold-threshold]"

SLIDER
0
650
200
683
infections-threshold
infections-threshold
0.01
1
0.8
0.01
1
Fraction
HORIZONTAL

SLIDER
0
310
190
343
days-until-healthy
days-until-healthy
0
366
365.0
1
1
Days/Ticks
HORIZONTAL

MONITOR
1180
460
1270
505
% Recovered
recovered-percentage
17
1
11

MONITOR
1275
460
1362
505
% Healthy
healthy-percentage
17
1
11

MONITOR
1365
460
1455
505
% Infected
infected-percentage
17
1
11

MONITOR
1461
460
1546
505
% Dead
dead-percentage
17
1
11

PLOT
1565
10
1855
265
Infection Rate
Ticks
Percentage
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"rate" 1.0 0 -16777216 true "" ""

@#$#@#$#@
# Global Disease Spread Model with Climate Impact

## WHAT IS IT?
This model simulates the spread of an infectious disease across a real-world map, taking into account geographic boundaries, population densities, and climate conditions. It uses GIS data to create a realistic representation of countries and implements various disease control strategies including isolation policies and transportation restrictions.

## HOW IT WORKS
The model operates on several key principles:

Geographic Setup:
   - Uses real-world GIS data for country boundaries
   - Assigns population density based on actual country data
   - Implements climate zones (hot, moderate, cold) affecting disease spread

Agent Behavior:
   - Agents (people) move within their countries
   - Can travel between countries via airports and ports
   - Carry disease states: healthy, infected, recovered, or dead
   - Respond to isolation measures based on compliance rates

Disease Mechanics:
   - Spreads through proximity between agents
   - Affected by temperature (different resistances in hot/cold regions)
   - Includes recovery and mortality rates
   - Features immunity period after recovery

Transportation System:
   - Airports and ports enable long-distance travel
   - Transport hubs can be enabled/disabled
   - Travel restrictions apply to isolated countries

## HOW TO USE IT

### Interface Controls:

Disease Parameters:
- infection-chance (0-1): Probability of disease transmission between agents
- mortality-rate (0-1): Probability of death for infected agents
- virus-heat-resistance (0-1): Disease effectiveness in hot regions
- virus-cold-resistance (0-1): Disease effectiveness in cold regions
- min-recovery-time (Days/Ticks): Minimum time before recovery possible
- recovery-time-mean (Days/Ticks): Average time until recovery
- recovery-time-sd (Days/Ticks): Standard deviation of recovery time
- days-until-healthy (Days/Ticks): Time before recovered agents become healthy

Travel Parameters:
- airports-and-ports-enabled? [switch]: Enables/disables transportation system
- num-airports (Units): Number of airports in simulation
- num-ports (Units): Number of ports in simulation
- airport-travel-chance (0-1): Probability of using airports
- port-travel-chance (0-1): Probability of using ports

Population Parameters:
- population-scaling (0-1): Adjusts total population size multiplied by the base scaling factor = 0.000005
- initial-infected (Agents): Starting number of infected agents
- movement-radius (Patches): How far agents can move per tick

Climate Parameters:
- heat-threshold (°C): Temperature above which regions are considered hot
- cold-threshold (°C): Temperature below which regions are considered cold

Disease Control Strategies:
- select-infection-country? [switch]: Allows manual selection of starting country
- isolation-enabled? [switch]: Enables country isolation mechanics
- isolation-compliance (0-1): Proportion of population following isolation rules
- compliance-rate (0-1): Individual compliance with movement restrictions
- infections-threshold (0-1): Infection rate triggering country isolation

### Monitors and Graphs:

Status Monitors:
- Most Affected Region: Shows which climate zone has most infections
- Most Infected: Country with highest infection count
- Most Deaths: Country with highest death count
- Transport Usage: Counts of airport and port usage

Population Statistics:
- Total Recovered/Healthy/Infected/Dead: Absolute numbers
- Percentages: Shows proportion in each health state

Graphs:
Infection Rate:
   - Y-axis: Percentage of population infected
   - X-axis: Time (ticks)
   - Shows overall infection trend

Daily New Cases:
   - Y-axis: Number of new infections
   - X-axis: Time (ticks)
   - Shows infection rate changes

Disease Progression:
   - Shows populations of each health state over time
   - Helps visualize epidemic curves

Climate Impact:
   - Compares disease spread in different climate zones
   - Helps understand environmental effects

## THINGS TO TRY
1. Compare disease spread with and without transportation
2. Test different isolation compliance rates
3. Start infections in different climate zones
4. Adjust virus resistance to temperature
5. Experiment with population density effects
6. Try different movement restrictions

## EXTENDING THE MODEL
1. Add vaccination mechanics
2. Implement multiple disease strains
3. Add economic impacts
4. Include seasonal temperature changes
5. Add more complex transportation networks
6. Implement contact tracing

## NETLOGO FEATURES
- Uses GIS extension for real-world geography
- Implements table extension for efficient data storage
- Uses turtle agentsets for optimized processing
- Employs patch caching for performance
- Uses custom plotting for multiple data visualization

## RELATED MODELS
- HIV
- Disease HubNet
- Virus
- Disease Solo
- Virus on a Network

## CREDITS AND REFERENCES
- World GIS Data: Natural Earth dataset
- Population Data: Natural Earth dataset
- Temperature Data: World Economics temperature data 10 years average
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
