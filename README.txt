INSTRUKCJA URUCHOMIENIA W SYSTEMIE WINDOWS - - - - - - - - - - - - 


INFORMACJE
1. Dane wejściowe powinny być plikiem w formacie csv. 
2. Pierwsza kolumna powinna reprezentować grupę. Aby przeprowadzić analizę korelacji potrzebne są conajmniej 2 grupy. 
3. W analizie korelacji zakłada się, że grupy są niezależne. 
4. Plik powinien zostać przejrzany pod kątem braku danych. 
5. W kolumnach nie numerycznych, w przypadku braku danych cały wiersz zostanie usunięty, ale można temu zapobiec wstawiając w taką kolumnę dowolny znak. 
6. Braki w kolumnach numerycznych nie stanowią problemu o ile w każdej kolumnie jest co najmniej jedna wartość dla danej grupy. 
7. W przypadku braku wywołania programu z argumentem określającym lokalizacje working directory, wyniki zostaną zapisane do folderu domyślnego
8. Wynikiem działania programu są 3 pliki pdf "Outliners", "Density", "Correlation" oraz plik raport.txt
9. Wszelkie warningi powinny zostać zignorowane.
10. W przypadku gdy grup jest na tyle dużo, że wykresy w pliku "outliers" są zbyt wąskie i nieczytelne, zaleca się usunąć 86 linie z pliku data_analysis.R