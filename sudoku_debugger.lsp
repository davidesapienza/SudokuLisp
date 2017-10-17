;sudoku


(defvar Sol)
;per aprire il file da GNU CLISP:
;(load "C:\\Users\\Davide\\Desktop\\LISP\\sudoku_debugger.lsp")

;TrovaElem ricerca un elemento nella lista
;parametri:
;	lista in cui cercare
;	numero da ricercare
;Ritorna:
;	NIL se l'elemento è nella lista
;	T altrimenti

;sono in maniera inversa rispetto al normale punto di vista logico, perchè andranno
;direttamente a comporre la lista di booleani appopriata.

;(defun TrovaElem (l num) (if (null l) T (if (equal num (car l)) NIL (TrovaElem (cdr l) num))))

(
defun TrovaElem (l num) 
	(
	if (null l)
		T
		(
		if (equal num (car l))
			NIL
			(
			TrovaElem (cdr l) num
			)
		)
	)
)

;ok
;es chiamata: (TrovaElem '(2 3 NIL NIL 5) 5)   --- (o anche NIL)
;--------------------------------------------------------------------------------
;CreaLista crea un lista di valori uguali (del valore passato) lunga quanto è lunga la lista passata
;parametri:
;	lista per sapere la lunghezza della lista da creare
;	val è il valore che ongi elemento della nuova lista assumerà
;Ritorna:
;	la lista di booleani (o tutta a T o tutta a NIL)

;(defun CreaLista (l val) (if (null l) NIL (append (CreaLista (cdr l) val) (list val))))

(
defun CreaLista (l val)
	(
	if (null l)
		NIL
		(
		append
		(CreaLista (cdr l) val)
		(list val)
		)
	)
)

;ok
;es chiamata: (CreaLista '(2 3 NIL NIL 5) T)  --(o NIL)
;--------------------------------------------------------------------------------
;SetRowBool combina le due funzioni: CreaLista e TrovaElem.
;parametri:
;	la lista in cui cercare l'elemento e da usare come riferimento per la sua lunghezza
;	num è il numero da ricercare
;ritorna:
;	una lista di booleani

;(defun SetRowBool (l num) (CreaLista l (TrovaElem l num)))

(
defun SetRowBool (l num)
	(CreaLista l (TrovaElem l num))
)
 
;ok
;es chiamata: (SetRowBool '(2 3 NIL NIL 5) 5)   
;--------------------------------------------------------------------------------
;SetRowBoolCorr filtra la riga dei booleani. Se il numero num lo posso mettere in quella particolare riga, devo
;comunque filtrarlo in base ai numeri già presenti.
;ottenuta la riga, devo individuare quegli elementi che sono già occupati
;e che quindi naturalmente non possono contenere il numero.
;parametri:
;	l lista dei numeri (riga del sudoku)
;	num il numero da inserire
;	lb lista dei booleani (a T le posizioni dove posso inserire il numero)
;ritorna:
;	la lista dei booleani corretta.

;(defun SetRowBoolCorr (l num lb) (if (not (null l)) (if (equal (car l) NIL) (append (list (car lb)) (SetRowBoolCorr (cdr l) num (cdr lb))) (append (list NIL) (SetRowBoolCorr (cdr l) num (cdr lb))))))

(
defun SetRowBoolCorr (l num lb)
	(if (not (null l))
		(
		if (equal (car l) NIL)
			(append (list (car lb)) (SetRowBoolCorr (cdr l) num (cdr lb)))
			(append (list NIL) (SetRowBoolCorr (cdr l) num (cdr lb)))
		)
	)
)

;OK
;es chiamata: (SetRowBoolCorr '(2 3 NIL NIL 4 NIL 7 NIL 8) 1 (SetRowBool '(2 3 NIL NIL 4 NIL 7 NIL 8) 1))
;--------------------------------------------------------------------------------
;SetMatrix applica il controllo della matrice per ogni riga.
; applica quindi la matrice SetRowBoolCorr ad ongi riga.
;il controllo quindi viene effettuato per righe.
;parametri:
;	la matrice dei numeri (numeri e NIL)
;	il numero da ricercare
;ritorna:
;	la matrice dei booleani

;(defun SetMatrix (m num) (if (not (null m)) (append (list (SetRowBoolCorr (car m) num (SetRowBool (car m) num))) (SetMatrix (cdr m) num))))

(
defun SetMatrix (m num)
	(
	if (not (null m))
		(append 
		(list (SetRowBoolCorr (car m) num (SetRowBool (car m) num)))
		(SetMatrix (cdr m) num)
		)
	)
)

;ok
;es: (SetMatrix (list (list 2 NIL NIL 3 NIL 7 NIL NIL 9) (list NIL 1 NIL NIL 9 NIL NIL 7 NIL)) 1)
;es: (SetMatrix (list (list 2 NIL NIL 3 NIL 7 NIL NIL 9)) 1)
;------------------------------------------------------------------------------
;RidimMatrix è una parte del compiot di trasposizione.
;ridimensiona la matrice da trasporre. la matrice risultante avrà per righe le righe della matrice originale,
;per colonne il numero delle olonne originarie meno uno. (toglie la prima colonna)
;parametri:
;	matrice originale
;ritorna:
;	matrice n x m-1


(
defun RidimMatrix (m)
	(if(not (null (cdr (car m))))
		(append (list (cdr (car m))) (RidimMatrix (cdr m))) 
	)
)

;ok 
;es: (RidimMatrix (list (list 2 3 4) (list 5 6 7)))
;------
;TrasponiStep prende ogni primo elemento delle liste di una matrice.
;in altre parole prende la prima colonna della matrice.
;parametri:
; 	la matrice originaria
;Ritona:
;	la prima colonna.

(
defun TrasponiStep (m)
	(	
	;la matrice è una lista di liste..
	;se non è vuota, allora prendi il primo elemento e appendilo
	;con la chiamata ricorsiva a Trasponi
	if (not (null m))
		(append (list (car (car m))) (TrasponiStep (cdr m)))
	)
)

;ok
;-----
;Trasponi produce la matrice trasposta di quella passata in input.
;parametri:
;	la matrice originaria;
;ritorna:
;	la matrice trasposta.

;l'idea è quella di trasporre la matrice così che si possono riutilizzare le funzioni di controllo per le righe sulle colonne.

(
defun Trasponi (m)
	(
	if (not (null m))
		(append (list (TrasponiStep m)) (Trasponi (RidimMatrix m)))
	)
)
;idea:
;prendo un elemento con car di car m,
;così prendo il primo elemento della riga
;questo lo appendo al primo elemento della seconda, ecc..
;di questo faccio una list, perchè voglio che la colonna
;sia la riga, e lo vedo come una lista..
;dopodichè mancano le altre colonne.. in questo caso, appendo 
;la coda della prima riga chiamando ricorsivamente la funzione dulla matrice
;composta da 

;ok 
;es: (Trasponi (list (list 2 3 4) (list 5 6 7)))
;--------------------------------------------------------------------------------
;MergeRow effettua l'end logico tra gli elementi di due liste.
;partametri:
;	le due liste.
;ritorna:
;	la lista con i risultati dell'end.

(
defun MergeRow (l1 l2)
	(
	if (not (null l1))
		(append (list (and (car l1) (car l2))) (MergeRow (cdr l1) (cdr l2)))
	)
)

;ok
;-------
;MergeMatrix effettua l'end logico tra gli elementi di due matrici.
;in particolare si appoggia a MergeRow.
;parametri:
;	le due matrici.
;Ritorna:
;	la matrice dei risultati dell'end.

(
defun MergeMatrix (m1 m2)
	(
	if (not (null m1))
		(append (list (MergeRow (car m1) (car m2))) (MergeMatrix (cdr m1) (cdr m2)))
	)
)

;ok
;------------------------------------------------------------------------
;ControlloRowCol permette di generare la matrice completa di tutti i booleani (posizioni ammissibili o non)
; effettua controlli per riga colonna e blocchi 3*3.
;dopo i singoli controlli fa il merge.
;(la trasposta due volte è per le colonne e per riottenere i risultati in colonna)
;parametri:
;	la matrice dei dati originali
;	il numero da ricercare
;ritorna:
;	la matrice dei booleani.

(
defun ControlloRowCol (m num)
		(MergeMatrix (MergeMatrix 
			(SetMatrix m num) (Trasponi (SetMatrix (Trasponi m) num)))
			(CheckBlock m num)
		)
)

;ok
; es:(ControlloRowCol (list (list 2 NIL NIL 3 NIL 7 NIL NIL 9) (list NIL 1 NIL NIL 9 NIL NIL 7 NIL)) 1)		
; (ControlloRowCol (list (list 2 NIL NIL 3 NIL 7 NIL NIL 9) (list NIL 1 NIL NIL 9 NIL NIL 7 NIL) (list 3 NIL 9 NIL 8 2 NIL NIL 5) (list 2 NIL NIL 3 NIL 7 NIL NIL 9) (list NIL 8 NIL NIL 9 NIL NIL 7 NIL) (list 3 NIL 9 NIL 8 2 NIL NIL 5)) 1)
;--------------------------------------------------------------------------
;SpezzaFascia prende le prime tre colonne della matrice del sudoku.
;in particolare questa funzione viene chiamata su una matrice m x 3,
;frutto della trasposizione della 3 x m di CheckBlockParz. 
;SpezzaFascia quindi della matrice m x 3 prende le prime tre righe.
;ottiene così una matrice 3 x 3.
;parametri:
;	la matrice m x 3
;	il numero che gli serve solo per richiamare CheckBlockParz
;	il valore che gli serve solo per la chiamati di CheckBlockParz
;ritorna:
;	ritorna una riga 1 x m con i valori di ritorno di tre chiamate a CheckBlockParz.

(
defun SpezzaFascia (m num val)
	(
	if (not (null m))
		(append (CheckBlockParz (append (list (car m)) (list (car (cdr m))) (list (car (cdr (cdr m))))) num val)
				(SpezzaFascia (cdr (cdr (cdr m))) num val))
		
	)
)

;ok;
;-----------
;CheckBlockParz ha due compiti:
;	- funzione di passo intermedio per ricavare la matrice 3*3.
;	- funzione di controllo della presenza dell'elemento nella matrice 3 x 3.
;Parametri:
;	la matrice in esame: al passo 1 sarà la 3 x m, al passo 2 sarà una 3 x 3.
;	il numero da ricercare nella 3 x 3.
;	val indica il passo attuale (o 1 o 2).
;ritorna:
;	una lista di booleani (prodotta da CreaLista) di 3 elem.
;	tutta NIL se l'elem è presente, a T se non è presente. 
;il ritorno di una 1 x 3 viene passata a spezza fascia.


(
defun CheckBlockParz (m num time)
	(progn
		;(write m)
		(if (= 0 time)
			(progn
			;(write "it:")
			;(write m)
			(SpezzaFascia (Trasponi m) num 1)
			)
			;qui (ramo else) devo controllare che il numero non 
			;ci sia in nessuna riga e in nessuna colonna (ho blocchetti
			;da 3)
;			(CheckBlock m num)
			;potrei usare subnet
			(
			if (and T
				;(
				;subnet
				;	(lambda (x) (mapcar (lambda (y) (if (= num y) T NIL)) x))
				;	m
				;)))
			;	(every (lambda (x) (not (TrovaElem x num))) (Trasponi m)))
				(some (lambda (x) (not (TrovaElem x num))) (Trasponi m)))

				;se vero allora crea lista fi NIL
				;passo sempre car m tanto mi interessa solo per sapere
				;il numero di elementi. 
				(CreaLista (car m) NIL)
				;altrimenti T
				(CreaLista (car m) T)
			)
		)
	)
) 

;ok;
;---------
;CheckBlock effettua il controllo di una matrice per blocchi di 3 righe. 
;(chiamando poi chi di dovere per il controllo della 3 x 3)
;Parametri:
;	la matrice dei numeri (originale)
;	il numero da ricercare p'er blocchi
;Ritorna:
;	la matrice dei booleani in cui le righe in blocchi di tre sono identiche.

(
defun CheckBlock (m num)
	(
	progn
		;(write-line "volta")
;		(
;		if (= block 0)
;			(setq block 3)
;			(setq block (- block 1))
;		)
;		(
;		if (and (not (null m)) (equal val 0))
;			(append (list (CheckBlockRow (append (list (car m)) (list (car (cdr m))) (list (car (cdr (cdr m))))) num val))
;					(list (CheckBlockRow (append (list (car m)) (list (car (cdr m))) (list (car (cdr (cdr m))))) num val))
;					(list (CheckBlockRow (append (list (car m)) (list (car (cdr m))) (list (car (cdr (cdr m))))) num val))
;					(fun (cdr (cdr (cdr m))) num val))
;			
;		)
;appendo tre volte, una per riga del blocco (tanto sono uguali)
		(
		if (not (null m))
			(append (list (CheckBlockParz (append (list (car m)) (list (car (cdr m))) (list (car (cdr (cdr m))))) num 0))
					(list (CheckBlockParz (append (list (car m)) (list (car (cdr m))) (list (car (cdr (cdr m))))) num 0))
					(list (CheckBlockParz (append (list (car m)) (list (car (cdr m))) (list (car (cdr (cdr m))))) num 0))
					(CheckBlock (cdr (cdr (cdr m))) num))
			
		)
		
	)
)

;ok
; es:(CheckBlock (list (list 2 NIL NIL 3 NIL 7 NIL NIL 9) (list NIL 1 NIL NIL 9 NIL NIL 7 NIL) (list 3 NIL 9 NIL 8 2 NIL NIL 5) (list 2 NIL NIL 3 NIL 7 NIL NIL 9) (list NIL 1 NIL NIL 9 NIL NIL 7 NIL) (list 3 NIL 9 NIL 8 2 NIL NIL 5)) 1)		
;----------------------------------------------------------------------------------------------------


;----------------------------------------------------------------------------------------------------
;Da qui finisce la parte del controllo.. 


;devo iniziare a buttare giù un modo per assegnare i numeri
;per ora parto da 1 e vado in fila..
;poi assegno l'uno alla prima posizione libera..


;ContaPos conta il numero di T nella riga.
;Parametri:
;	la lista di booleani
;Ritorna:
;	il numero di T nella lista.

(
defun ContaPos(l)
	(
	if (not (null l))
		(
		;se è T
		if (car l)
			(+ 1 (ContaPos (cdr l)))
			(ContaPos (cdr l))
		)
		0
	)
)
;--------------------------------------------------------------------
;ContaIndiece trova l'indice in cui inserire il numero nella lista di 9 posizioni.
;Parametri:
;	la lista di booleani
;	il numero di T da far passare
;Ritorna:
;	il numero di corrispondente all'indice.

(
defun ContaIndiece(val lbool)
	(
	;finchè non è a zero il val allora richiama ricorsivamente
	;finchè non c'è un bool T allora richiama ricorsivamente
	if (not (= 0 val))
		(
		if (not (car lbool))
			(+ 1 (ContaIndiece val (cdr lbool)))
			(+ 1 (ContaIndiece (- val 1) (cdr lbool)))
		)
		;quando è zero allora scrivi al prossimo T
		(
		if (not (car lbool))
			(+ 1 ( ContaIndiece val (cdr lbool) ))
			0
		)
		
	)
)
;es:
;(ContaIndiece 4 (list T T NIL NIL T T))

;--------------------------------------------------------------------
;InserisciNum inserisce il numero num in l nella posizione individuata come:
;scorro lbool di val T, assegna poi il numero al prossimo T incontrato
;parametri:
;	val numero di T da passare nella lista di booleani
;	la lista dei numeri
;	la lista dei booleani con le posizioni ammesse
;	il numero da inserire
;ritorna:
;	la lista con il numero inserito.
(
defun InserisciNum (val l lbool num)
	(
	progn
	;	(write-line "entro in InserisciNum")
	;	(write-line "parametri: ")
	;	(write val)
	;	(write-line "")
	;	(write l)
	;	(write-line "")
	;	(write lbool)
	;	(write-line "")
	;	(write num)
	;	(write-line "")
		;finchè non è a zero il val allora richiama ricorsivamente
		;finchè non c'è un bool T allora richiama ricorsivamente
		(
		if (not (= 0 val))
			(
			if (not (car lbool))
				(append (list (car l)) (InserisciNum val (cdr l) (cdr lbool) num))
				(append (list (car l)) (InserisciNum (- val 1) (cdr l) (cdr lbool) num))
			)
			;quando è zero allora scrivi al prossimo T
			(
			if (not (car lbool))
				(append (list (car l)) (InserisciNum val (cdr l) (cdr lbool) num))
				(append (list num) (cdr l))
			)
			
		)
	)
)
;es:
;(InserisciNum 3 (list 1 2 3 4 5 6) (list NIL T T T NIL T) 10)
;ok
;--------------------------------------------------------------------
;RisolviNum...
;parametri:
;
;ritorno:
;

;passaggi che devo fare:
;ricavo la matrice dei booleani
;prendo la prima riga, conto le posizioni utili,
;prendo a caso un numero tra 1 e numero posizioni utili, 
;inserisco lì il numero, passo alla riga successiva
;(
;defun RisolviNum (mOrig m mBool num)
;	(
;	progn
		;a <-- numero posizioni a T
;		(setq a (ContaPos (car mBool)))
		;(write-line "")
		;(write a)
		;se non sono arrivato alla fine della matrice
;		(
;		if (not (null m))
			
			
;			(
			;se il numero di posizioni a T non è zero allora posso inserire il numero..
;			if (not (= 0 a))
;				(progn
					;cosa devo fare?
					;inserisco il numero, poi passo alla riga successiva..
					;se tale ricorsione mi ritorna true allora ok ho inserito tutti i numeri..
					;se invece mi ritorna false, allora devo togliere il numero nella posizione in cui lo ho inserito.. 
					;settare tale posizione a false (cioè non poò più essere scelta)
					;e scegliere un'altra strada..
					;(InserisciNum (random a) (car m) (car (ControlloRowCol m num)) num)
;					(setq rand (random a))
					;(write rand)
;					(
;					if (not (RisolviNum (cdr mOrig) (cdr m) (cdr (ControlloRowCol (append (list (InserisciNum rand (car m) (car mBool) num)) (cdr m)) num)) num))
							;caso in cui devo togliere tale posizione.. dalla matrice bool
							;posso riutilizzare InserisciNum inserendo nella riga car il NIL
							;richiamo RisolviNum con stessi parametri e con matrice dei Bool modificata
;							(RisolviNum mOrig m (append (list (InserisciNum rand (car mBool) (car mBool) NIL)) (cdr mBool)) num)
;					)
					;altrimenti torna insietro senza fare niente.
;				)
				
				;se a = 0 allora o c'è già l'uno presente nella riga, o ha 
				;finito le posizioni ammissibili e deve tornare indietro ricorsivamente
				
;				(
;				if (not (TrovaElem (car mOrig) num))
					;altrimenti deve andare avanti
					;nota:la ,matrice dei bool è già aggiornata. c'era già il numero.
;					(RisolviNum (cdr mOrig) (cdr m) (cdr mBool) num)
					;se ritorna T vuol dire che non c'è, se ritorna NIL vuol dire che c'è
;					;quindi se non c'è allora deve ritornare false
;					NIL	
;				)
;			)
;			
;		)
;	)
;)

;commento imp!!!!
;Il random crea un numero casuale tra zero e num-1..
;il gioco però funziona..
;se ho 4 soli T, allora mi crea un numero tra 0 e 3..
;nella funzione InserisciNum, sottrae uno ad ogni passaggio al numero casuale
;e questo va bene.. quando è zero allora vuol dire che è al primo T, e sovrascrive,
;ecc ecc


;(
;defun RisolviNum (mOrig m mBool num)
;	(
;	if (not (null m))
;		(
;		progn
;			(setq a (ContaPos (car mBool)))
;			;se è zero
;			(if  (= 0 a)
;				;se a = 0 ci sono due casi.. o perchè il numero è presente in mOrig
;				;o perchè non ci sono più posti possibili (scelte passate sbagliate)
;				(
;				if (not (TrovaElem (car mOrig) num))
;					;il not perchè vai a leggere il funzionamento di trova elem.
;					;quindi se l'elem è presente in mOrig allora vai avanti
;					;non devo ricalcolarmi la matrice dei booleani e nemmeno inserire l'elemento
;					;in questo caso quindi appendo la prima riga della matrice dei numeri.
;					(append (car m)(RisolviNum (cdr mOrig) (cdr m) (cdr mBool) num))
;					
;					;altrimenti c'è qualcosa che toppa, torna su e cambia ricorsivamente.
;					NIL
;				)
;				;altrimenti devo inserire il numero
;				(
;				progn
;					(setq rand (random a)) ;trovo la posizione casuale dove inserirlo
;					(
;					if (RisolviNum (cdr mOrig) 
;									(cdr m) 
;									(cdr (ControlloRowCol 
;											(append (list (InserisciNum rand (car m) (car mBool) num)) (cdr m))
;											num
;										)
;									)
;									num
;						)
;						;se vero allora appendi la riga in testa.
;						(append (list (InserisciNum rand (car m) (car mBool) num)) 
;							(RisolviNum (cdr mOrig) (cdr m)
;									(cdr (ControlloRowCol 
;											(append (list (InserisciNum rand (car m) (car mBool) num)) (cdr m))
;											num
;										)
;									)
;									num
;							)
;						)
;						;se è falso allora cambia posizione. quella attuale non è più valida.
;						;richiama la funzione con semplicemente la matrice dei booleani con la posizione attuale a NIL.
;						(RisolviNum mOrig m (append (list (InserisciNum rand (car mBool) (car mBool) NIL)) (cdr mBool)) num)
;					)
;					
;				)
;			)
;		)
;		T
;	)
;)


;

;

;TrovaRiga ritorna la riga corretta.
;data una matrice e un indice di riga, scorre la matrice finchè non trova la riga corrispondente all'indice.
;Parametri:
;	la matrice
; 	l'indice
;ritorna:
;	la riga corretta. 

(
defun TrovaRiga (m row)
	(
	progn
		;(write-line "entro in TrovaRiga")
		(
		if (not (= row 0))
			(TrovaRiga (cdr m) (- row 1))
			(car m)
		)
	)
)

;(TrovaRiga (list (list 2 3 4) (list 1 2 3) (list 6 5 4)) 2)
;ok
;--------
;MatrixChangeRow ritorna la matrice con una sola riga modificata.
;Parametri:
;	la matrice
;	la nuova riga
;	l'indice della nuova riga
;Ritorna:
;	la nuova matrice risultante dalla concatenazione della matrice vecchia, la nuova riga e il resto della matrice vecchia.

(
defun MatrixChangeRow (m newR rowid)
	(
	progn
		;(write-line "entro in MatrixChangeRow")
		(
		if (not (= rowid 0))
			(append (list (car m)) (MatrixChangeRow (cdr m) newR (- rowid 1)))
			(append (list newR) (cdr m))
		)
		
	)
)
;es:
;(MatrixChangeRow (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list 1 1 1) 2)
;ok
;---------

;incf

;CONTROLLI
;(ControlloRowCol (list (list 2 NIL NIL 3 NIL 7 NIL NIL 9) (list NIL 1 NIL NIL 9 NIL NIL 7 NIL) (list 3 NIL 9 NIL 8 2 NIL NIL 5) (list 2 NIL NIL 3 NIL 7 NIL NIL 9) (list NIL 8 NIL NIL 9 NIL NIL 7 NIL) (list 3 NIL 9 NIL 8 2 NIL NIL 5)) 1)


(
defun RisolviSudoku (mOrig m mBool num row)
	;se ho già inserito tutti i numeri allor mi fermo
	(
	progn
		(write-line "parametri: ")
		(write num)
		(write-line "")
		(write row)	
		(write-line "")
		(write m)
		(write-line "")
		(write mBool)
		(write-line "")
		
		(
		if (= 10 num)
			(
			progn
				(write "fine")
				(write m)
				(setq Sol m)
				(return)
			)
		
			;se per questo numero ho già risolto tutta la matrice, allora riparti con il prossimo numero
			(
			if (= 9 row)
				(
				progn
					;(write num)
					;(write-line " fine con numero")
					(RisolviSudoku mOrig m (ControlloRowCol m (+ 1 num)) (+ 1 num) 0)
				)
				(
				progn
					;altrimenti deve cercare di mettere il numero
					(setq a (ContaPos (TrovaRiga mBool row)))
					;(write a)
					;(write-line " trovo a")
					
					;se è zero
					(
					if  (= 0 a)
						;se a = 0 ci sono due casi.. o perchè il numero è presente in mOrig
						;o perchè non ci sono più posti possibili (scelte passate sbagliate)
						(
						if (not (TrovaElem (TrovaRiga mOrig row) num))
							;il not perchè vai a leggere il funzionamento di trova elem.
							;quindi se l'elem è presente in mOrig allora vai avanti
							;non devo ricalcolarmi la matrice dei booleani e nemmeno inserire l'elemento
							(RisolviSudoku mOrig m mBool num (+ 1 row))
							
							;altrimenti c'è qualcosa che toppa, torna su e cambia ricorsivamente.
							NIL
						)
					;
						;altrimenti devo inserire il numero
						(
						progn
							(setq rand (random a)) ;trovo la posizione casuale dove inserirlo
							;(write rand)
							;(write-line " trovo rand")
							;(write (TrovaRiga m row))
							;(write-line "")
							;(write (TrovaRiga mBool row))
							;(write-line "")
							(
							if (not (RisolviSudoku mOrig 
											(MatrixChangeRow m 
															(InserisciNum 
																rand 
																(TrovaRiga m row)
																(TrovaRiga mBool row) 
																num
															) 
															row
											)
											(ControlloRowCol 
															(MatrixChangeRow m 
																			(InserisciNum 
																				rand 
																				(TrovaRiga m row) 
																				(TrovaRiga mBool row) 
																				num
																			) 
																			row
															) 
															num
											)
											num
											(+ 1 row)
									)
								)
								(
								progn
									;(write-line "occhio devo tornare indietro")
									;se è falso allora cambia posizione. quella attuale non è più valida.
									;richiama la funzione con semplicemente la matrice dei booleani con la posizione attuale a NIL.
									(RisolviSudoku 
										mOrig 
										m 
										(MatrixChangeRow mBool (InserisciNum rand (TrovaRiga mBool row) (TrovaRiga mBool row) NIL) row) 
										num 
										row
									)
									;(InserisciNum 3 (list T NIL T NIL T T T NIL) (list T NIL T NIL T T T NIL) NIL)
									;(MatrixChangeRow (list (list NIL T T) (list T T T) (list NIL NIL T)) (InserisciNum 1 (TrovaRiga (list (list NIL T T) (list T T T) (list NIL NIL T)) 1) (TrovaRiga (list (list NIL T T) (list T T T) (list NIL NIL T)) 1) NIL) 1)
								)
								;(write-line "ok")
							)
							;(write-line "chiamato if")
							
						)
					)
				)
			)
		)
	)
)


;-----
;Sudoku permette la risoluzione del sudoku passato in input
;Parametri:
;	il sudoku da risolvere :matrice
;Ritorna:
;	il sudoku corretto

(
defun Sudoku (S)
	(
	progn
		(setq Sol NIL)
		(RisolviSudoku S S (ControlloRowCol S 1) 1 0)
		Sol
	)
)



