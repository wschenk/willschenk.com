#!/bin/bash
#!/bin/bash

# Starlink Status Monitor
# Monitors Starlink connection and reports status changes

# Configuration
STARLINK_IP="192.168.100.1"
STARLINK_PORT="9200"
CHECK_INTERVAL=5
LOG_FILE="/tmp/starlink_monitor.log"

# State tracking
PREVIOUS_STATE=""
PREVIOUS_PING_QUALITY=""
PREVIOUS_SPEED_QUALITY=""
PREVIOUS_GPS_QUALITY=""
PREVIOUS_OBSTRUCTION_QUALITY=""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging function
log_event() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "$LOG_FILE"
}

# Check if grpcurl is available
check_dependencies() {
    if ! command -v grpcurl &> /dev/null; then
        echo -e "${RED}❌ grpcurl is not installed. Please install it with: brew install grpcurl${NC}"
        exit 1
    fi
    
    if ! command -v jq &> /dev/null; then
        echo -e "${RED}❌ jq is not installed. Please install it with: brew install jq${NC}"
        exit 1
    fi
}

# Query Starlink API
query_starlink() {
    timeout 10 grpcurl -plaintext -d '{"get_status":{}}' "$STARLINK_IP:$STARLINK_PORT" SpaceX.API.Device.Device/Handle 2>/dev/null
}

# Determine connection status
get_connection_status() {
    local json="$1"
    
    # Check if there's an outage
    local outage_cause
    outage_cause=$(echo "$json" | jq -r '.dishGetStatus.outage.cause // empty')
    
    if [ -n "$outage_cause" ]; then
        case "$outage_cause" in
            "BOOTING")
                echo "BOOTING"
                ;;
            "NO_PINGS")
                echo "NETWORK_ISSUES"
                ;;
            *)
                echo "SERVICE_ISSUE"
                ;;
        esac
    else
        echo "ONLINE"
    fi
}

# Get ping quality
get_ping_quality() {
    local json="$1"
    local latency
    latency=$(echo "$json" | jq -r '.dishGetStatus.popPingLatencyMs // -1')
    
    if [ "$latency" = "-1" ] || [ "$latency" = "null" ]; then
        echo "NO_PING"
    elif [ "$(echo "$latency < 30" | bc -l 2>/dev/null || echo 0)" -eq 1 ]; then
        echo "STRONG"
    elif [ "$(echo "$latency < 60" | bc -l 2>/dev/null || echo 0)" -eq 1 ]; then
        echo "GOOD"
    else
        echo "WEAK"
    fi
}

# Get speed quality
get_speed_quality() {
    local json="$1"
    local down_bps up_bps down_mbps
    down_bps=$(echo "$json" | jq -r '.dishGetStatus.downlinkThroughputBps // 0')
    down_mbps=$(echo "scale=1; $down_bps / 1000000" | bc -l 2>/dev/null || echo "0")
    
    if [ "$(echo "$down_mbps > 100" | bc -l 2>/dev/null || echo 0)" -eq 1 ]; then
        echo "EXCELLENT"
    elif [ "$(echo "$down_mbps > 50" | bc -l 2>/dev/null || echo 0)" -eq 1 ]; then
        echo "GOOD"
    else
        echo "LIMITED"
    fi
}

# Get GPS quality
get_gps_quality() {
    local json="$1"
    local gps_valid gps_sats
    gps_valid=$(echo "$json" | jq -r '.dishGetStatus.gpsStats.gpsValid // false')
    gps_sats=$(echo "$json" | jq -r '.dishGetStatus.gpsStats.gpsSats // 0')
    
    if [ "$gps_valid" = "true" ] && [ "$gps_sats" -gt 10 ]; then
        echo "STRONG"
    elif [ "$gps_valid" = "true" ] && [ "$gps_sats" -ge 5 ]; then
        echo "MODERATE"
    else
        echo "WEAK"
    fi
}

# Get obstruction quality
get_obstruction_quality() {
    local json="$1"
    local fraction
    fraction=$(echo "$json" | jq -r '.dishGetStatus.obstructionStats.fractionObstructed // 0')
    local percentage
    percentage=$(echo "scale=1; $fraction * 100" | bc -l 2>/dev/null || echo "0")
    
    if [ "$(echo "$percentage < 2" | bc -l 2>/dev/null || echo 1)" -eq 1 ]; then
        echo "CLEAR"
    elif [ "$(echo "$percentage < 10" | bc -l 2>/dev/null || echo 0)" -eq 1 ]; then
        echo "PARTIALLY_BLOCKED"
    else
        echo "OBSTRUCTED"
    fi
}

# Format status with emoji and color
format_status() {
    local status="$1"
    case "$status" in
        "ONLINE")
            echo -e "${GREEN}🛰️  ONLINE${NC}"
            ;;
        "BOOTING")
            echo -e "${YELLOW}🔄 BOOTING${NC}"
            ;;
        "NETWORK_ISSUES")
            echo -e "${RED}📡 NETWORK ISSUES${NC}"
            ;;
        "SERVICE_ISSUE")
            echo -e "${RED}⚠️  SERVICE ISSUE${NC}"
            ;;
        "ERROR")
            echo -e "${RED}❌ CONNECTION ERROR${NC}"
            ;;
    esac
}

# Format quality indicators
format_quality() {
    local type="$1"
    local quality="$2"
    local value="$3"
    
    case "$quality" in
        "STRONG"|"EXCELLENT"|"CLEAR")
            echo -e "${GREEN}🟢 $type: $value${NC}"
            ;;
        "GOOD"|"MODERATE"|"PARTIALLY_BLOCKED")
            echo -e "${YELLOW}🟡 $type: $value${NC}"
            ;;
        *)
            echo -e "${RED}🔴 $type: $value${NC}"
            ;;
    esac
}

# Main monitoring function
monitor_starlink() {
    local json response status ping_quality speed_quality gps_quality obstruction_quality
    local changed=false
    
    # Query the API
    response=$(query_starlink)
    
    if [ $? -eq 0 ] && [ -n "$response" ]; then
        json="$response"
        status=$(get_connection_status "$json")
        ping_quality=$(get_ping_quality "$json")
        speed_quality=$(get_speed_quality "$json")
        gps_quality=$(get_gps_quality "$json")
        obstruction_quality=$(get_obstruction_quality "$json")
    else
        status="ERROR"
        ping_quality="NO_PING"
        speed_quality="LIMITED"
        gps_quality="WEAK"
        obstruction_quality="UNKNOWN"
    fi
    
    # Check for changes
    if [ "$status" != "$PREVIOUS_STATE" ]; then
        log_event "Status changed: $(format_status "$PREVIOUS_STATE") → $(format_status "$status")"
        changed=true
    fi
    
    if [ "$ping_quality" != "$PREVIOUS_PING_QUALITY" ]; then
        local latency
        if [ "$status" != "ERROR" ]; then
            latency=$(echo "$json" | jq -r '.dishGetStatus.popPingLatencyMs // "N/A"')
            log_event "$(format_quality "Network" "$ping_quality" "${latency}ms")"
        fi
        changed=true
    fi
    
    if [ "$speed_quality" != "$PREVIOUS_SPEED_QUALITY" ]; then
        if [ "$status" != "ERROR" ]; then
            local down_bps up_bps down_mbps up_mbps
            down_bps=$(echo "$json" | jq -r '.dishGetStatus.downlinkThroughputBps // 0')
            up_bps=$(echo "$json" | jq -r '.dishGetStatus.uplinkThroughputBps // 0')
            down_mbps=$(echo "scale=1; $down_bps / 1000000" | bc -l 2>/dev/null || echo "0")
            up_mbps=$(echo "scale=1; $up_bps / 1000000" | bc -l 2>/dev/null || echo "0")
            log_event "$(format_quality "Speed" "$speed_quality" "↓${down_mbps} ↑${up_mbps} Mbps")"
        fi
        changed=true
    fi
    
    if [ "$gps_quality" != "$PREVIOUS_GPS_QUALITY" ]; then
        if [ "$status" != "ERROR" ]; then
            local gps_sats
            gps_sats=$(echo "$json" | jq -r '.dishGetStatus.gpsStats.gpsSats // 0')
            log_event "$(format_quality "GPS" "$gps_quality" "${gps_sats} satellites")"
        fi
        changed=true
    fi
    
    if [ "$obstruction_quality" != "$PREVIOUS_OBSTRUCTION_QUALITY" ]; then
        if [ "$status" != "ERROR" ]; then
            local fraction percentage
            fraction=$(echo "$json" | jq -r '.dishGetStatus.obstructionStats.fractionObstructed // 0')
            percentage=$(echo "scale=1; $fraction * 100" | bc -l 2>/dev/null || echo "0")
            log_event "$(format_quality "Sky View" "$obstruction_quality" "${percentage}% obstructed")"
        fi
        changed=true
    fi
    
    # Update previous states
    PREVIOUS_STATE="$status"
    PREVIOUS_PING_QUALITY="$ping_quality"
    PREVIOUS_SPEED_QUALITY="$speed_quality"
    PREVIOUS_GPS_QUALITY="$gps_quality"
    PREVIOUS_OBSTRUCTION_QUALITY="$obstruction_quality"
    
    # Show current summary if there were changes
    if [ "$changed" = true ]; then
        echo -e "\n${BLUE}📊 Current Status Summary:${NC}"
        format_status "$status"
        if [ "$status" != "ERROR" ]; then
            local latency down_bps up_bps down_mbps up_mbps gps_sats fraction percentage
            latency=$(echo "$json" | jq -r '.dishGetStatus.popPingLatencyMs // "N/A"')
            down_bps=$(echo "$json" | jq -r '.dishGetStatus.downlinkThroughputBps // 0')
            up_bps=$(echo "$json" | jq -r '.dishGetStatus.uplinkThroughputBps // 0')
            down_mbps=$(echo "scale=1; $down_bps / 1000000" | bc -l 2>/dev/null || echo "0")
            up_mbps=$(echo "scale=1; $up_bps / 1000000" | bc -l 2>/dev/null || echo "0")
            gps_sats=$(echo "$json" | jq -r '.dishGetStatus.gpsStats.gpsSats // 0')
            fraction=$(echo "$json" | jq -r '.dishGetStatus.obstructionStats.fractionObstructed // 0')
            percentage=$(echo "scale=1; $fraction * 100" | bc -l 2>/dev/null || echo "0")
            
            format_quality "Network" "$ping_quality" "${latency}ms"
            format_quality "Speed" "$speed_quality" "↓${down_mbps} ↑${up_mbps} Mbps"
            format_quality "GPS" "$gps_quality" "${gps_sats} satellites"
            format_quality "Sky View" "$obstruction_quality" "${percentage}% obstructed"
        fi
        echo ""
    fi
}

# Signal handlers for clean shutdown
cleanup() {
    log_event "Monitoring stopped by user"
    exit 0
}

trap cleanup SIGINT SIGTERM

# Main execution
main() {
    echo -e "${BLUE}🛰️  Starlink Status Monitor${NC}"
    echo "Monitoring Starlink at $STARLINK_IP:$STARLINK_PORT"
    echo "Log file: $LOG_FILE"
    echo "Press Ctrl+C to stop"
    echo ""
    
    check_dependencies
    log_event "Starlink monitoring started"
    
    while true; do
        monitor_starlink
        sleep "$CHECK_INTERVAL"
    done
}

# Run the monitor
main "$@"
