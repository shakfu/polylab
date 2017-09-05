docker-compose up -d
sleep 5

echo "STARTING ======================================================================"
docker-compose run go
sleep 1
echo "ENDING   ======================================================================"
docker-compose down

