#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include <limits.h>
#include <assert.h>

#define n (0)
#define d (1)
#define Generation (2)
#define Population (3)
#define Words (4)							//the maximum number of codes in salmon
#define MemoryProbability (0)
#define ReproductionFraction (1)
#define RouletteSelection (2)
#define IM (3)

float var[5]={0};
float ratio[4]={0};

/*
//code size
#define n (6)			//Codeword length
#define d (3)			//Distance
#define Words (400)
#define vocab (4096)     //(int)((double)pow ((double)4, (double)n));

// constants
#define Generation (20)
#define Population (400)
#define MemoryProbability (0.7)
#define ReproductionFraction (0.5)
#define RouletteSelection (0.5)
#define IM (5)
*/

//set an integer array to a specific value
void set (int *arr, int val, int len)
{
	int i;
	for (i=0; i<len; ++i)
	{
		arr[i] = val;
	}
	return;
}


//set a 2 d integer array to a specific value
void set2 (int **arr, int val, int len1, int len2)
{
	int i, j;
	for (i=0; i<len1; ++i)
	{
		for (j=0; j<len2; ++j)
		{
			arr[i][j] = val;
		}
	}
	return;
}


//converting numbers to a quaternary alphabet
void convert (int dec, char *s)
{
	int i=0;
	int *quat;				//code length
	double q=0;

	quat=calloc (var[n], sizeof(int));	


	//convert number base 10 to a number base 4
	while(i<var[n])
	{

         quat[i]= dec % 4;
		 q=q+((quat[i])* ((float) pow((float) 10,i)));
		 switch (quat[i])
		 {
			case 0:	s[(int)var[n]-i-1] = 'A';
				break;
			case 1:	s[(int)var[n]-i-1] = 'C';
				break;
			case 2:	s[(int)var[n]-i-1] = 'G';
				break;
			case 3:	s[(int)var[n]-i-1] = 'T';
				break;
		 }
		 i=++i;
		 dec = dec / 4;
	}

	free(quat);
	return;


}


//calculate levistain distance between two codewords
int levidist (int w1, int w2)
{
	//lavensthein variables
	char *word1;
	char *word2;
	int **matrix;
	int dlete;
	int insert;
	int substitute;
	int minimum;
	int i;
	int j;
	char c1;
	char c2;

	word1=calloc (var[n], sizeof(char));
	word2=calloc (var[n], sizeof(char));

	matrix  = (int **)calloc(var[n]+1, sizeof(int *));
	if(matrix == NULL)
	{
		fprintf(stderr, "out of memory\n");
	}
	for(i = 0; i < var[n]+1; i++)
	{
		matrix[i] = (int *)calloc(var[Words], sizeof(int));
		if(matrix[i] == NULL)
		{
			fprintf(stderr, "out of memory\n");
		}
	}


	//converts unused vector to base4
	convert(w1, &word1[0]);
	convert(w2, &word2[0]);


	//Levensthein distance code taken from http://www.lemoda.net/c/levenshtein/ on 30/12/2015
	for (i = 0; i <= var[n]; ++i)
	{
		matrix[i][0] = i;
	}
	for (i = 0; i <= var[n]; ++i)
	{
		matrix[0][i] = i;
	}
	for (i = 1; i <= var[n]; ++i) {

		c1 = word1[i-1];
		for (j = 1; j <= var[n]; ++j) {

			c2 = word2[j-1];
			if (c1 == c2) {
				matrix[i][j] = matrix[i-1][j-1];
			}
			else {

				dlete = matrix[i-1][j] + 1;
				insert = matrix[i][j-1] + 1;
				substitute = matrix[i-1][j-1] + 1;
				minimum = dlete;
				if (insert < minimum) {
					minimum = insert;
				}
				if (substitute < minimum) {
					minimum = substitute;
				}
				matrix[i][j] = minimum;
			}
		}
	}

	j = matrix[(int)var[n]][(int)var[n]];

	free(word1);
	free(word2);
	for(i = 0; i < var[n]+1; i++)
		free(matrix[i]);
	free(matrix);

	return j;
}


//salmon algorithm
int salmon()
{
	//declare counters and variables
	int gen, salmon, memory, tabu, i;
	int un, can;
	int r;
	double random;
	double sum;
	double maxprob;
	double curprob;
	int probpos;
	int pop=(int)var[Population];
	int temp;
	int margin;
	int max;
	int maxcan;
	int dist;
	int min;

	//salmon algortihm variables
	int **salmont;
	int **salmonm;
	int *unused;
	int *candidates;
	int *salmonsort;
	int *waterlevel;
	int *salmong;
	int *salmonp;
	//elite salmon
	int elitepos = 0;


	//int salmont[Population][Words] = {0};
	salmont = (int **)calloc(var[Population], sizeof(int *));
	if(salmont == NULL)
	{
		fprintf(stderr, "out of memory\n");
	}
	for(i = 0; i < var[Population]; i++)
	{
		salmont[i] = (int *)calloc(var[Words], sizeof(int));
		if(salmont[i] == NULL)
		{
			fprintf(stderr, "out of memory\n");
		}
	}

	//int salmonm[Population][Words] = {0};
	salmonm  = (int **)calloc(var[Population], sizeof(int *));
	if(salmonm == NULL)
	{
		fprintf(stderr, "out of memory\n");
	}
	for(i = 0; i < var[Population]; i++)
	{
		salmonm[i] = (int *)calloc(var[Words], sizeof(int));
		if(salmonm[i] == NULL)
		{
			fprintf(stderr, "out of memory\n");
		}
	}


	//int unused[Words];
	unused = (int *)malloc(var[Words] * sizeof(int));
	if(unused == NULL)
	{
		fprintf(stderr, "out of memory\n");
	}

	//int candidates[var[Words]] = {0};
	candidates = (int *)malloc(var[Words]* sizeof(int));
	if( candidates== NULL)
	{
		fprintf(stderr, "out of memory\n");
	}

	//int waterlevel[vocab]
	waterlevel = (int *)malloc(powl(4,var[n])* sizeof(int));
	if(waterlevel == NULL)
	{
		fprintf(stderr, "out of memory\n");
	}

	//int salmong[Population]
	salmong = (int *)malloc(var[Population]* sizeof(int));
	if(salmong == NULL)
	{
		fprintf(stderr, "out of memory\n");
	}

	//int salmonsort[Population]
	salmonsort = (int *)malloc(var[Population]* sizeof(int));
	if(salmonsort == NULL)
	{
		fprintf(stderr, "out of memory\n");
	}

	salmonp = (int *)malloc(var[Population]* sizeof(int));
	if(salmonp == NULL)
	{
		fprintf(stderr, "out of memory\n");
	}




	// Intializes random number generator
	srand(time(NULL));

	
	set2 (salmont, -1, (int)var[Population], (int)var[Words]);
	
	set2 (salmonm, -1, (int)var[Population], (int)var[Words]);
	set (unused, -1, (int)var[Words]);
	set (candidates, -1, (int)var[Words]);
	set (waterlevel, (int)ratio[IM]*var[Words], powl(4,var[n]));
	set (salmonp, (int)var[Words], (int)var[Population]);

	//Initialise all salmon
	for (salmon=0; salmon<(int)var[Population]; ++salmon)
	{
		memory =0;
		//place random clique in memory
		for (memory=0; memory < (int)var[Words]; ++memory)
		{
			do
			{
				r = rand();
			} while (r >= powl(4,(int)var[n]));
			salmonm[salmon][memory] = r;
		}
	}



	// repeat for all generations of salmon
	for (gen=0; gen<(int)var[Generation]; ++gen)
	{
		printf("  %d   ", gen);
		for (salmon=0; salmon<pop; ++salmon)
		{
			tabu = 0;
			un = 0;
			can = 0;

			//take every vector in memory and check the probability that the vector is utilised by probability
			for (memory=0; memory< salmonp[salmon]; ++memory)
			{
				random=((double)rand()/(double)RAND_MAX);

				//if utilised the vector is stored in tabu list
				if ((salmonm[salmon][memory] >= 0) && (random<ratio[MemoryProbability]))
				{
					salmont[salmon][tabu] = salmonm[salmon][memory];
					++tabu;
				}
				else
				{
					unused[un]= salmonm[salmon][memory];
					++un;
				}
			}

			/*
			i=0;
			for (memory=0; memory < (int)var[Words]; ++memory)
			{

				printf("\n%d %d", memory, salmont[salmon][memory]);
				if (salmont[salmon][memory]<0)
				{
					printf("  %d", unused[i]);
					++i;
				}

			}*/



			//create candidates using the unused vectors
			for (i=0; i< un; ++i)
			{
				min = INT_MAX;

				//calculate the distance of each possible candidate with the words present in the tabu list
				for (memory=0; memory< salmonp[salmon]; ++memory)
				{
					dist = levidist(unused[i], salmont[salmon][memory]);

					if (min>dist)
					{
						min=dist;
					}
				}
				// checks if the vector is a candidate
				if ((unused[i] >= 0) && (min>=(int)var[d]))
				{
					candidates[can] = unused[i];
					++can;
				}
			}
			maxcan = can;

			do
			{
				maxprob = 0;
				probpos = 0;
				//choose a vector from candidate with the highest probability
				for (can=0; can< maxcan; ++can)
				{

					if (candidates[can] >=0)
					{
						//calculate Probability
						sum = 0;
						i =0;
						for (i=0;i<powl(4,(int)var[n]);++i)
						{
							sum = sum+((double)pow ((double)waterlevel[i], (double)ratio[RouletteSelection]));
						}

						curprob = ((double)pow ((double)waterlevel[candidates[can]], (double)ratio[RouletteSelection])/sum);

						//compare probability with the highest probability
						if (curprob > maxprob)
						{
							maxprob=curprob;
							probpos=can;
						}
					}
				}

				//select candidate with the highest probability and place it in tabu list
				salmont[salmon][tabu] = candidates[probpos];
				++tabu;
				//remove candiate from list
				i=0;
				for  (can=0; can <= maxcan; ++can)
				{
					if (can != probpos)
					{
						candidates[i]=candidates[can];
						++i;
					}
				}
			} while (candidates[0] >=0);

		}

		//count the size of each salmon
		for (salmon=0; salmon<pop; ++salmon)
		{
			i =0;
			for (tabu=0; tabu<(int)var[Words]; ++tabu)
			{
				if (salmont[salmon][tabu]>=0)
				{
					++i;
				}
			}
			salmong[salmon] = i;
		}

		//update water levels
		for (salmon=0; salmon<pop; ++salmon)
		{
			for (tabu=0; tabu<salmong[salmon]; ++tabu)
			{
				if (salmont[salmon][tabu]>=0)
				{
					waterlevel[salmont[salmon][tabu]]+=salmong[salmon];
				}
			}
		}

		//duplicate array
		for (salmon = 0; salmon < pop; ++salmon)
		{
			salmonsort[salmon]=salmong[salmon];
		}

		//sorting salmon
		for (salmon = 0; salmon < pop; ++salmon)
		{
			for (i = salmon + 1; i < pop; ++i)
			{
				if (salmonsort[salmon] > salmonsort[i])
				{
					temp =  salmonsort[salmon];
					salmonsort[salmon] = salmonsort[i];
					salmonsort[i] = temp;
				}
			}
		}
		margin = (int)pop*ratio[ReproductionFraction];

		//reproduce
		set2 (salmonm, -1, (int)var[Population], (int)var[Words]);
		max=salmonsort[pop-1];
		i=0;
		for (salmon = 0; salmon < pop; ++salmon)
		{
			if ((salmong[salmon]>=salmonsort[margin]) || (pop<20))				//allows a population of 10 salmon to survive after all generations
			{
				//move tabu list to memory
				for (memory=0; memory<(int)var[Words]; ++memory)
				{
					salmonm[i][memory]=salmont[salmon][memory];
				}
				if (salmong[salmon] == max)
				{
					elitepos = i;
				}
				++i;
			}
			else
			{
				set (&salmonm[salmon][0], -1, (int)var[Words]);
			}
		}

		//update group pop
		set (salmonp, -1, var[Population]);
		for (salmon = 0; salmon < pop; ++salmon)
		{
			salmonp[salmon]=salmong[salmon];
		}

		//reset lists
		set2 (salmont, -1, var[Population], var[Words]);
		set (salmong, -1, var[Population]);
		set (salmonsort, -1, var[Population]);

		pop = i; //update population

		/*
//not part of the alogorithm, outputs the current best salmon
		if (gen>5) 
		{
			r = screencheck(max, elitepos, salmonm);
		}
		*/


	}//loop for each generation

	r = textcheck(salmonp, pop, salmonm);

	for(i = 0; i < (int)var[Population]; i++)
		free(salmont[i]);
	free(salmont);
	for(i = 0; i < (int)var[Population]; i++)
		free(salmonm[i]);
	free(salmonm);
	free(unused);
	free(candidates);
	free(waterlevel);
	free(salmong);
	free(salmonsort);
	free(salmonp);

	return max;
}


//check and print to screen
int screencheck(int max, int elitepos, int **salmonm)
{
	int un;
	int i;
	int dist;
	int memory;
	int min;
	char *word1;

	word1=calloc (var[n], sizeof(char));

	min = 100;

	//check and print the code
	for (un=0; un < max; ++un)
	{
		//converts code  vector to base4
		convert(salmonm[elitepos][un], word1);

		//print codeword
		for (i=0; i<var[n]; ++i)
		{
			printf("%c",word1[i]);
		}
		printf(" ");

		//loop through the other codes and checks their
		for (memory=un+1; memory< max; ++memory)
		{

			dist = levidist(salmonm[elitepos][un], salmonm[elitepos][memory]);

			//print distance
			printf("%d ",dist);

			if (min>dist)
			{
				min=dist;
			}
		}

		printf("\n");

	}
	// checks min distance
	if (min < var[d])
	{
		printf("\n Error in codebook");
	} else
	{
		printf("\n Biggest codebook has %d codes of length %d and minimum distance %d ",max, (int)var[n], (int)var[d]);
	}

	free(word1);

	return min;
}

//enter information through screen
void screenmenu()
{
	//set codeword size
	printf("\nEnter codeword size - ");
	while(scanf("%f",&var[n]) != 1)
    {
        printf("Please enter an integer: ");
        while(getchar() != '\n');
    }

	// set codeword distance
    printf("\nEnter codeword distance - ");
	while(scanf("%f",&var[d]) != 1)
    {
        printf("Please enter an integer: ");
        while(getchar() != '\n');
    }

	//set salmon generations
	printf("\nEnter salmon generations - ");
	while(scanf("%f",&var[Generation]) != 1)
    {
        printf("Please enter an integer: ");
        while(getchar() != '\n');
    }

	//set salmon population
	printf("\nEnter salmon population - ");
	while(scanf("%f",&var[Population]) != 1)
    {
        printf("Please enter an integer: ");
        while(getchar() != '\n');
    }

	//set salmon memory size
	printf("\nEnter salmon memory size - ");
	while(scanf("%f",&var[Words]) != 1)
    {
        printf("Please enter an integer: ");
        while(getchar() != '\n');
    }

	// set memory probability
	ratio[MemoryProbability] = 0.7;

	// set reproduction fraction
	ratio[ReproductionFraction] = 0.5;

	// set roulette selection
	ratio[RouletteSelection] = 0.5;

	//set IM
	ratio[IM] = 5;
}

//load information from text file
void textmenu()
{    
	int i;

	FILE *myFile;
    myFile = fopen("Text.txt", "r");

	assert(myFile != NULL);

    for (i = 0; i < 5; i++)
    {
        fscanf(myFile, "%f", &var[i]);
    }

    for (i = 0; i < 5; i++)
    {
        printf("Number is: %f\n\n", var[i]);
    }

	fclose(myFile);

	
	// set memory probability
	ratio[MemoryProbability] = 0.7;

	// set reproduction fraction
	ratio[ReproductionFraction] = 0.5;

	// set roulette selection
	ratio[RouletteSelection] = 0.5;

	//set IM
	ratio[IM] = 5;

	return;
}

//check salmon code and print to text file
int textcheck(int *salmong, int pop, int **salmonm)
{
	int p;
	int un;
	int i;
	int dist;
	int memory;
	int min;
	char *word1;

	FILE *myFile;
    myFile = fopen("Text.txt", "a");

	assert(myFile != NULL);

	word1=calloc (var[n], sizeof(char));


	for (p=0; p<pop; ++p)
	{
		min = INT_MAX;

		//loop through all the codes and check
		for (un=0; un < salmong[p]; ++un)
		{
			for (memory=un+1; memory< salmong[p]; ++memory)
			{

				dist = levidist(salmonm[p][un], salmonm[p][memory]);
				if (min>dist)
				{
					min=dist;
				}
			}

		}
		// checks min distance
		if (min >= var[d])
		{
			//print the code in file
			for (un=0; un < salmong[p]; ++un)
			{
				//converts code  vector to base4
				convert(salmonm[p][un], word1);

				//print codeword
				for (i=0; i<var[n]; ++i)
				{
					 fprintf(myFile, "%c",word1[i]);
				}
				printf(" ");

				//loop through the other codes and checks their
				for (memory=un+1; memory< salmong[p]; ++memory)
				{

					dist = levidist(salmonm[p][un], salmonm[p][memory]);

					//print distance
					fprintf(myFile," %d",dist);
/*
					if (min>dist)
					{
						min=dist;
					}*/
				}

				fprintf(myFile,"\n");

			}
			// writes the information

			fprintf(myFile,"\n Codebook has %d codes of length %d and minimum distance %d \n\n",salmong[p], (int)var[n], (int)var[d]);

		}
	}

	free(word1);
	fclose(myFile);
	return min;
}

int main()
{
	int dis;

	textmenu();
	//textcheck();

	//screenmenu();

	dis = salmon();


	return 0;

}