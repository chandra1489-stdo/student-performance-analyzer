#!/usr/bin/env bash
set -euo pipefail

DATA_DIR="${APP_DATA_DIR:-/var/data}"
mkdir -p "$DATA_DIR" "$DATA_DIR/photos" /app/www

for csv in students_db.csv users_db.csv announcements_db.csv daily_attendance_db.csv timetable_db.csv; do
  if [ ! -f "$DATA_DIR/$csv" ] && [ -f "/app/$csv" ]; then
    cp "/app/$csv" "$DATA_DIR/$csv"
  fi
done

if [ -d /app/www/photos ]; then
  find /app/www/photos -maxdepth 1 -type f -exec cp -n {} "$DATA_DIR/photos/" \;
  rm -rf /app/www/photos
fi

ln -sfn "$DATA_DIR/photos" /app/www/photos

exec Rscript /app/start-render.R
