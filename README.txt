INSTRUKCJA URUCHOMIENIA W SYSTEMIE WINDOWS - - - - - - - - - - - - 
"ścieżka do pliku R.exe" CMD BATCH --vanilla "--args nazwa_pliku.csv ścieżla do zapisu" Data_analysis.R

>Podanie ścieżki do folderu jako drugi argument jest opcjonalne. W przypadku jego braku folder zostanie ustawiony domyślnie. Do takiego folderu będą zapisywane wyniki.
>Jeśli nie jest się w folderze który zawiera plik Data_analysis.R oraz nasze dane, trzeba podać pełną ścieżkę.

INFORMACJE
1. Dane wejściowe powinny być plikiem w formacie csv. 
2. Pierwsza kolumna powinna reprezentować grupę. Aby przeprowadzić analizę korelacji potrzebne są conajmniej 2 grupy. 
3. W analizie różnicowej podczas przeprowadzania testów (Anova,Kruskal,Wilcoxon,Welch,T_Student) zakłada się, że grupy są niezależne. 
4. Plik powinien zostać przejrzany pod kątem braku danych. 
5. W kolumnach nie numerycznych, w przypadku braku danych cały wiersz zostanie usunięty, ale można temu zapobiec wstawiając w taką kolumnę dowolny znak. 
6. Braki w kolumnach numerycznych nie stanowią problemu o ile w każdej kolumnie jest co najmniej jedna wartość dla danej grupy. 
7. Wynikiem działania programu są 3 pliki pdf "Outliners", "Density", "Correlation" oraz plik raport.txt
8. Wszelkie warningi powinny zostać zignorowane.
9. W przypadku gdy grup jest na tyle dużo, że wykresy w pliku "outliers" są zbyt wąskie i nieczytelne, zaleca się usunąć 86 linie z pliku data_analysis.R