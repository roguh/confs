#!/bin/sh

get_icon() {
    case $1 in
        01d) icon="";;
        01n) icon="";;
        02d) icon="";;
        02n) icon="";;
        03*) icon="";;
        04*) icon="";;
        09d) icon="";;
        09n) icon="";;
        10d) icon="";;
        10n) icon="";;
        11d) icon="";;
        11n) icon="";;
        13d) icon="";;
        13n) icon="";;
        50d) icon="";;
        50n) icon="";;
        *) icon="";
    esac

    echo $icon
}

get_duration() {

    osname=$(uname -s)

    case $osname in
        *BSD) date -r "$1" +"$TIMEFORMAT";;
        *) date --date="@$1" +"$TIMEFORMAT";;
    esac

}

TIMEFORMAT="%l:%M %p"
UNITS="Imperial"
# Set KEY and CITYID
. ~/.config/polybar/secrets
# SYMBOL="°"
SYMBOL=""

if [ ! -z $CITYID ]; then
    current=$(curl -sf "http://api.openweathermap.org/data/2.5/weather?APPID=$KEY&id=$CITYID&units=$UNITS")
    forecast=$(curl -sf "http://api.openweathermap.org/data/2.5/forecast?APPID=$KEY&id=$CITYID&units=$UNITS&cnt=1")
else
    location=$(curl -sf https://location.services.mozilla.com/v1/geolocate?key=geoclue)

    if [ ! -z "$location" ]; then
        location_lat="$(echo "$location" | jq '.location.lat')"
        location_lon="$(echo "$location" | jq '.location.lng')"

        current=$(curl -sf "http://api.openweathermap.org/data/2.5/weather?appid=$KEY&lat=$location_lat&lon=$location_lon&units=$UNITS")
        forecast=$(curl -sf "http://api.openweathermap.org/data/2.5/forecast?APPID=$KEY&lat=$location_lat&lon=$location_lon&units=$UNITS&cnt=1")
    fi
fi

if [ ! -z "$current" ] && [ ! -z "$forecast" ]; then
    current_temp=$(echo "$current" | jq ".main.temp" | cut -d "." -f 1)
    current_icon=$(echo "$current" | jq -r ".weather[].icon")

    forecast_temp=$(echo "$forecast" | jq ".list[].main.temp" | cut -d "." -f 1)
    forecast_icon=$(echo "$forecast" | jq -r ".list[].weather[].icon")


    if [ "$current_temp" -gt "$forecast_temp" ]; then
        trend=""
    elif [ "$forecast_temp" -gt "$current_temp" ]; then
        trend=""
    else
        trend=""
    fi


    sun_rise=$(echo "$current" | jq ".sys.sunrise")
    sun_set=$(echo "$current" | jq ".sys.sunset")
    now=$(date +%s)

    if [ "$sun_rise" -gt "$now" ]; then
        daytime=" $(get_duration "$((sun_rise-now))")"
    elif [ "$sun_set" -gt "$now" ]; then
        daytime=" $(get_duration "$((sun_set-now))")"
    else
        daytime=" $(get_duration "$((sun_rise-now))")"
    fi

    # Convert meters per second (mps) to mph
    wind_speed=$(echo $current | jq ".wind.speed" | awk '{printf("%.1f", $1 * 2.23694)}')
    wind_dir=$(echo $current | jq ".wind.deg")
    wind="$wind_speed mph"
    echo "$daytime $wind  $(get_icon \"$current_icon\") $current_temp$SYMBOL  $trend  $(get_icon \"$forecast_icon\") $forecast_temp$SYMBOL"
fi
