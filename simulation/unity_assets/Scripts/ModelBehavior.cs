using System.Collections.Generic;
using UnityEngine;

public class ModelBehavior : MonoBehaviour
{

    // Model and simulation parameters
    public float RunID;
    public string OutputPath = "/Users/benjamda/Dropbox/Research/Active/CR-Collectives/Simulation output/";
    public int MaxCommunitySize;
    public int InitialPreyPopulationSize;
    public int InitialPredPopulationSize;
    public int InitialMutantPopulationSize;
    public float TimeStep;
    public float BirthRate;
    public float CarryingCapacity;
    public float InteractionRadius;                                         // Orientation and alignment radius. Must be greater than or equal to other radii. 
    public float RepulsionRadius;						                    // Avoidance of conspecifics radius 
	public float AttractionRadius;											// Attraction to conspecifics radius
	public float AlignmentRadius;											// Alignment with conspecifics radius
    public float CaptureRadius;                                             // Prey capture radius
    public float MaxTurnRate;
    public float CaptureRate;
    public float MutantAdvantage;                                           // Advantage (or disadvantage) of a second predator type - (mutant capture rate)/(wildtype capture rate)
    public float ConversionProbability;
    public float DeathRate;
    public float HandlingTime;
    public float MetabolicRate;							                    // Rate at which predator energy decays toward equilibrium
    public float EquilibriumEnergy;                                         // Predator energy at equilibrium
    public float PreyNoise;                                                 // Standard deviation of prey angular acceleration
    public float PredNoise;                                                 // Standard deviation of pred angular acceleration
    public float ArenaLength;                                               // Length of side of square arena
    public int ArenaLengthInBuckets;					                    // For spatial hashing, the length of the arena in buckets. 
    public float PreySpeed;
    public float PredSpeed;
    public bool CollectivePred;
    public bool CollectivePrey;
    public bool Pursuit;
    public bool Avoidance;
    public bool DeviantMutant;                                               // Does the mutant have the opposite setting on collective behavior to the wildtype?

    // Main state variables
    [HideInInspector] public float[] LocationX;
    [HideInInspector] public float[] LocationY;
    [HideInInspector] public float[] VelocityX;
    [HideInInspector] public float[] VelocityY;
    [HideInInspector] public bool[] IsPrey;
    [HideInInspector] public bool[] IsActive;
    [HideInInspector] public bool[] IsHandling;
    [HideInInspector] public bool[] IsMutant;
    [HideInInspector] public int FocalPred;                                 // First predator the algorithm encounters
    [HideInInspector] public List<int> InFocalCaptureRange;                 // List of prey individuals in capture range of focal predator
    [HideInInspector] public float SimulationTime;                          // Current time 
    [HideInInspector] public int SimulationStep;                            // Current step number
    [HideInInspector] public int PreyPopulationSize;
    [HideInInspector] public int PredPopulationSize;                        // Total number of predators
    [HideInInspector] public int MutantPopulationSize;                      // Number of predators that are mutants
    [HideInInspector] public float Polarization;
    [HideInInspector] public float LocalConspecifics;                       // Conspecifics in interaction range of each predator, summed across all predators 
    [HideInInspector] public float LocalConspecificsQuesting;               // Conspecifics in interaction range of each predator, summed across predators who are questing
    [HideInInspector] public float LocalConspecificsHandling;               // Conspecifics in interaction range of each predator, summed across predators who are handling
    [HideInInspector] public float LocalConspecificsMutant;                 // Conspecifics in interaction range of each mutant, summed across all mutants
    [HideInInspector] public float LocalHeterospecificsMutant;              // Heterospecifics in interaction range of each mutant, summed across all mutants
    [HideInInspector] public float Crowding;                                // Lloyd's mean crowding calculated from LocalConspecifics - the expected number of conspecifics in range of a random predator
    [HideInInspector] public float CrowdingQuesting;                        // Lloyd's mean crowding calculated from LocalConspecificsQuesting - the expected number of conspecifics in range of a random predator who is questing
    [HideInInspector] public float CrowdingHandling;                        // Lloyd's mean crowding calculated from LocalConspecificsHandling - the expected number of conspecifics in range of a random predator who is handling
    [HideInInspector] public int PreyInCaptureRange;                        // Prey in capture range, summed across all predators
    [HideInInspector] public int PreyInCaptureRangeQuesting;                // Prey in capture range, summed across predators who are questing
    [HideInInspector] public int PreyInCaptureRangeHandling;                // Prey in capture range, summed across predators who are handling
    [HideInInspector] public int PreyInCaptureRangeMutant;                  // Prey in capture range, summed across predators who are mutants
    [HideInInspector] public int PreyConsumed;
    [HideInInspector] public int PredHandling;
    [HideInInspector] public int PreyInBucketNeighborhood;
    [HideInInspector] public float[] Energy;                                // For each predator, increases with consumption, decreases according to metabolic rate: a moving average of consumption rate
    [HideInInspector] public int[] Bucket;				                    // For spatial hashing, stores which bucket on the landscape each individual is currently in
    [HideInInspector] public float BucketConversionFactor;                  // For spatial hashing, converting from spatial coordinates to bucket index (without using division)
    [HideInInspector] public float BucketLength;                            // For spatial hashing, the width of a square bucket. Must be greater than or equal to interaction radius. 
    [HideInInspector] public Dictionary<int, List<int>> BucketDict;         // For spatial hashing, a dictionary with key = bucket, value = list of individuals in that bucket
    [HideInInspector] public Dictionary<int, List<int>> NeighbourDict;      // For predator group tracking, a dictionary with key = individual, value = list of individuals they have behavioral interactions with
    [HideInInspector] public Dictionary<int, List<int>> GroupDict;          // For predator group tracking, a dictionary with key = group index, value = list of individuals in that group
    [HideInInspector] public bool[] InFocalGroup;                           // For predator group tracking, is an individual in the focal group
    [HideInInspector] public int NumberOfGroups;                            // For predator group tracking, total number of groups including mutants
    [HideInInspector] public int NumberOfMutantGroups;                      // For predator group tracking, number of mutant groups    
    [HideInInspector] public int MaxGroupSize;
    [HideInInspector] public float MeanGroupSize;
    [HideInInspector] public float VarGroupSize;
    [HideInInspector] public int NumberOfSingletonGroups;


    // Important internal variables
    private float[] NextLocationX;
    private float[] NextLocationY;
    private float[] NextVelocityX;
    private float[] NextVelocityY;
    private float SumVelocityX;
    private float SumVelocityY;
    private Vector2 SumVelocity;
    private float InteractionRadiusSquared;
    private float RepulsionRadiusSquared;
	private float AttractionRadiusSquared;
	private float AlignmentRadiusSquared;
    private float CaptureRadiusSquared;
    private float MaxTurnAngle;
    private float MetabolicRateScaled;
    private float PreyBirthProbability;
    private float CaptureProbability;
    private float AdjustedCaptureProbability;                               // Incorporating potential effects of mutant advantage
    private float HandlingTimeinSteps;
    private float FinishHandlingProbability;
    private float DeathProbability;
    private float HalfArenaLength;                                          // Maximum horiztonal or vertical distance individuals could be apart given periodic boundary conditions
    private int nBucket;								                    // For spatial hashing, the number of buckets
    private float theta;                                                    // Generic angle
    private float mag;    								                    // Generic vector magnitude
    private bool IsRepulsed;                                                // Does a predator have conspecifics within the range of repulsion
    private int gridx;                                                      // For spatial hashing, the horizontal component of a bucket's coordinates
    private int gridy;                                                      // For spatial hashing, the vertical component of a bucket's coordinates
    private List<int> InteractingWithi;                                     // For predator group tracking, a list of individuals that individual i has behavioral interactions with



    // Advance the model one time step
    public void Step()
    {

        SimulationStep++;

        // Recalculate these constants each step 
        // to allow adjustments in Unity Editor during simulation
        InteractionRadiusSquared = InteractionRadius * InteractionRadius;
        RepulsionRadiusSquared = RepulsionRadius * RepulsionRadius;
		AttractionRadiusSquared = AttractionRadius * AttractionRadius;
		AlignmentRadiusSquared = AlignmentRadius * AlignmentRadius;
        CaptureRadiusSquared = CaptureRadius * CaptureRadius;
        MaxTurnAngle = MaxTurnRate * TimeStep;
        HandlingTimeinSteps = 1 + HandlingTime / TimeStep;                  // Adding 1 to handling steps to allow handling times of 0 (in which case pred stops handling next timestep with probability 1)
        FinishHandlingProbability = 1 / HandlingTimeinSteps;
        CaptureProbability = CaptureRate * TimeStep;
        DeathProbability = DeathRate * TimeStep;
        MetabolicRateScaled = MetabolicRate * TimeStep;
        HalfArenaLength = ArenaLength / 2f;


        // Density dependence in prey
        PreyBirthProbability = BirthRate * (1 - PreyPopulationSize / CarryingCapacity) * TimeStep;


        // Reset population monitors
        PreyPopulationSize = 0;
        PredPopulationSize = 0;
        MutantPopulationSize = 0;
        SumVelocityX = 0f;
        SumVelocityY = 0f;
        PreyInBucketNeighborhood = 0;
        PreyInCaptureRange = 0;
        PreyInCaptureRangeQuesting = 0;
        PreyInCaptureRangeHandling = 0;
        PreyInCaptureRangeMutant = 0;
        LocalConspecifics = 0;
        LocalConspecificsQuesting = 0;
        LocalConspecificsHandling = 0;
        LocalConspecificsMutant = 0;
        LocalHeterospecificsMutant = 0;
        PreyConsumed = 0;
        PredHandling = 0;
        NumberOfGroups = 0;
        MeanGroupSize = 0f;
        VarGroupSize = 0f;
        MaxGroupSize = 0;
        NumberOfSingletonGroups = 0;
        NumberOfMutantGroups = 0;


        // Clear group dictionaries and arrays
        InFocalCaptureRange.Clear();
        NeighbourDict.Clear();
        GroupDict.Clear();
        InFocalGroup = new bool[MaxCommunitySize];


        // Rebuild spatial hash
        UpdateBucketDict();



        // Loop through individuals to determine their next states
        for (int i = 0; i < MaxCommunitySize; i++)
        {

            // Only active individuals are considered
            if (IsActive[i])
            {

                // Extract this individual's location and velocity
                float LocationXi = LocationX[i];
                float LocationYi = LocationY[i];
                float VelocityXi = VelocityX[i];
                float VelocityYi = VelocityY[i];


                // Starting values for next location and velocity
                float NextLocationXi = LocationXi;
                float NextLocationYi = LocationYi;
                float NextVelocityXi = VelocityXi;
                float NextVelocityYi = VelocityYi;
                float RepulsionVelocityXi = 0f;         // Repulsion velocity replaces NextVelocity for setting velocity for next time step
                float RepulsionVelocityYi = 0f;         // iff there are conspecifics within the radius of repulsion



                // Prey updating
                if (IsPrey[i])
                {


                    // Increment prey population size
                    PreyPopulationSize++;



                    // Reset some repulsed flag for each prey
                    IsRepulsed = false;



                    // Prey birth (prey death is handled during predator updating)
                    if (Random.value < PreyBirthProbability)
                    {


                        // Find and assign an inactive index to new prey, and intialize their location and velocity
                        // If there are no inactive indices then FindInactiveIndex returns -1
                        // and this birth just doesn't happen (and there is a warning)
                        int k = FindInactiveIndex();
                        if (k >= 0)
                        {
                            IsActive[k] = true;
                            IsPrey[k] = true;
                            IsHandling[k] = false;

                            LocationX[k] = LocationXi;
                            LocationY[k] = LocationYi;

                            NextLocationX[k] = LocationXi;
                            NextLocationY[k] = LocationYi;

                            theta = Random.value * 2 * Mathf.PI;
                            NextVelocityX[k] = Mathf.Cos(theta) * PreySpeed;
                            NextVelocityY[k] = Mathf.Sin(theta) * PreySpeed;

                            Energy[k] = 0f;

                        }
                        else
                        {
                            Debug.Log("Warning: maximum community size reached");
                        }

                    }
                    // End prey birth




                    // Prey behavioral interactions
                    if (Avoidance || CollectivePrey)
                    {

                        gridx = (int)(LocationXi * BucketConversionFactor);
                        gridy = (int)(LocationYi * BucketConversionFactor);
                        int x;
                        int y;
                        int bucket;
                        List<int> neighbors = new List<int>();
                        for (int ix = gridx - 1; ix <= gridx + 1; ix++)
                        {

                            x = ix;

                            if (ix == -1) { x = ArenaLengthInBuckets - 1; }         // Periodic boundary conditions 
                            if (ix == ArenaLengthInBuckets) { x = 0; }              // on horizontal bucket neighbors

                            for (int iy = gridy - 1; iy <= gridy + 1; iy++)
                            {

                                y = iy;

                                if (iy == -1) { y = ArenaLengthInBuckets - 1; }     // Periodic boundary conditions 
                                if (iy == ArenaLengthInBuckets) { y = 0; }          // on vertical bucket neighbors

                                bucket = x + y * ArenaLengthInBuckets;
                                neighbors = BucketDict[bucket];

                                int j;
                                for (int ineighbor = 0; ineighbor < neighbors.Count; ineighbor++)
                                {

                                    j = neighbors[ineighbor];

                                    // Interactions only occur for nonself, active individuals
                                    if (j == i) continue;
                                    if (IsActive[j])
                                    {


                                        // Calculating horizontal and vertical displacements
                                        // adjusting for boundary effects
                                        float dx = LocationX[j] - LocationXi;
                                        if (dx > HalfArenaLength)
                                        {
                                            dx -= ArenaLength;
                                        }

                                        if (dx < -HalfArenaLength)
                                        {
                                            dx += ArenaLength;
                                        }


                                        float dy = LocationY[j] - LocationYi;
                                        if (dy > HalfArenaLength)
                                        {
                                            dy -= ArenaLength;
                                        }

                                        if (dy < -HalfArenaLength)
                                        {
                                            dy += ArenaLength;
                                        }



                                        // Interactions only occur for in-range individuals
                                        float dsq = dx * dx + dy * dy;
                                        if (dsq < InteractionRadiusSquared)
                                        {

                                            if (IsPrey[j])
                                            {

                                                if (CollectivePrey)
                                                {

                                                    // Repulsion - first priority, if they are a nonzero distance away
                                                    if (dsq < RepulsionRadiusSquared)
                                                    {

                                                        IsRepulsed = true;

                                                        if (dsq > 0)
                                                        {
                                                            mag = Mathf.Sqrt(dsq);
                                                            RepulsionVelocityXi -= dx / mag;
                                                            RepulsionVelocityYi -= dy / mag;
                                                        }

                                                    }

                                                    // Attraction and alignment - while no repulsive behavior has been triggered 
                                                    if (!IsRepulsed)
                                                    {

                                                        // Attraction, if they are a nonzero distance away
														if (dsq < AttractionRadiusSquared & dsq > 0)
                                                        {
                                                            mag = Mathf.Sqrt(dsq);
                                                            NextVelocityXi += dx / mag;
                                                            NextVelocityYi += dy / mag;
                                                        }

                                                        // Alignment
														if (dsq < AlignmentRadiusSquared) 
														{
															NextVelocityXi += VelocityX [j];
															NextVelocityYi += VelocityY [j];
														}


                                                    }

                                                }

                                            }


                                            if (!IsPrey[j])
                                            {

                                                if (Avoidance)
                                                {

                                                    mag = Mathf.Sqrt(dsq);
                                                    if (mag > 0)
                                                    {
                                                        NextVelocityXi -= dx / mag;
                                                        NextVelocityYi -= dy / mag;
                                                    }

                                                }


                                            }


                                        }

                                    }
                                }
                            }
                        }
                    }
                    // End prey behavioral interactions 


                    if (IsRepulsed)
                    {
                        NextVelocityXi = RepulsionVelocityXi;
                        NextVelocityYi = RepulsionVelocityYi;
                    }


                    // Convert next velocity to angle (the 'desired direction'), add noise
                    float DesiredDirection = Mathf.Atan2(NextVelocityYi, NextVelocityXi);
                    DesiredDirection += Rnormal() * PreyNoise * TimeStep;


                    // Temper desired direction by max turn angle
                    float Direction = Mathf.Atan2(VelocityYi, VelocityXi);
                    float NextDirection = Mathf.Deg2Rad * Mathf.MoveTowardsAngle(Direction * Mathf.Rad2Deg,
                                                                                 DesiredDirection * Mathf.Rad2Deg,
                                                                                 MaxTurnAngle * Mathf.Rad2Deg);

                    // Convert back to velocity
                    NextVelocityXi = Mathf.Cos(NextDirection) * PreySpeed;
                    NextVelocityYi = Mathf.Sin(NextDirection) * PreySpeed;



                }
                // End prey updating, begin predator updating
                else
                {

                    // Initialize group list for this predator
                    // and record self for equivalence relation
                    InteractingWithi = new List<int>();
                    InteractingWithi.Add(i);


                    // Focal predator is first predator encountered
                    if (PredPopulationSize == 0)
                    {
                        FocalPred = i;
                    }

                    // Increment predator population size and global summed velocities
                    PredPopulationSize++;
                    SumVelocityX += VelocityXi;
                    SumVelocityY += VelocityYi;
                    if (IsMutant[i])
                    {
                        MutantPopulationSize++;
                    }





                    // Reset repulsed flag for each predator
                    IsRepulsed = false;




                    // Adjusted capture probability, factoring in potential mutant advantage
                    AdjustedCaptureProbability = CaptureProbability;
                    if (IsMutant[i])
                    {
                        AdjustedCaptureProbability = MutantAdvantage * CaptureProbability;
                    }





                    // Predator energy consumption
                    Energy[i] = (1 - MetabolicRateScaled) * (Energy[i] - EquilibriumEnergy) + EquilibriumEnergy;




                    // Predator death
                    if (Random.value < DeathProbability)
                    {
                        IsActive[i] = false;
                    }




                    // If the predator was handling they may finish this time step
                    if (IsHandling[i])
                    {

                        PredHandling++;

                        if (Random.value < FinishHandlingProbability)
                        {

                            IsHandling[i] = false;

                        }
                    }



                    // Predator interactions with other individuals
                    gridx = (int)(LocationXi * BucketConversionFactor);
                    gridy = (int)(LocationYi * BucketConversionFactor);
                    int x;
                    int y;
                    int bucket;
                    List<int> neighbors = new List<int>();

                    for (int ix = gridx - 1; ix <= gridx + 1; ix++)
                    {

                        x = ix;

                        if (ix == -1) { x = ArenaLengthInBuckets - 1; }         // Periodic boundary conditions 
                        if (ix == ArenaLengthInBuckets) { x = 0; }              // on horizontal bucket neighbors

                        for (int iy = gridy - 1; iy <= gridy + 1; iy++)
                        {

                            y = iy;

                            if (iy == -1) { y = ArenaLengthInBuckets - 1; }     // Periodic boundary conditions 
                            if (iy == ArenaLengthInBuckets) { y = 0; }          // on vertical bucket neighbors

                            bucket = x + y * ArenaLengthInBuckets;
                            neighbors = BucketDict[bucket];

                            int j;
                            for (int ineighbor = 0; ineighbor < neighbors.Count; ineighbor++)
                            {

                                j = neighbors[ineighbor];

                                // Interactions only occur for nonself, active individuals
                                if (j == i) continue;
                                if (IsActive[j])
                                {

                                    // Tally prey in neighborhood
                                    if (IsPrey[j])
                                    {
                                        PreyInBucketNeighborhood++;
                                    }


                                    // Calculating horizontal and vertical displacements
                                    // adjusting for boundary effects
                                    float dx = LocationX[j] - LocationXi;
                                    if (dx > HalfArenaLength)
                                    {
                                        dx -= ArenaLength;
                                    }

                                    if (dx < -HalfArenaLength)
                                    {
                                        dx += ArenaLength;
                                    }


                                    float dy = LocationY[j] - LocationYi;
                                    if (dy > HalfArenaLength)
                                    {
                                        dy -= ArenaLength;
                                    }

                                    if (dy < -HalfArenaLength)
                                    {
                                        dy += ArenaLength;
                                    }



                                    // Interactions only occur for in-range individuals
                                    float dsq = dx * dx + dy * dy;
                                    if (dsq < InteractionRadiusSquared)
                                    {

                                        // Begin predator-prey interactions
                                        if (IsPrey[j])
                                        {

                                            // Pursuit, if they are a nonzero distance away
                                            if (Pursuit)
                                            {

                                                mag = Mathf.Sqrt(dsq);
                                                if (mag > 0)
                                                {
                                                    NextVelocityXi += dx / mag;
                                                    NextVelocityYi += dy / mag;
                                                }

                                            }


                                            // Capture range 
                                            if (dsq < CaptureRadiusSquared)
                                            {

                                                PreyInCaptureRange++;

                                                if (i == FocalPred)
                                                {
                                                    InFocalCaptureRange.Add(j);
                                                }


                                                if (IsMutant[i])
                                                {
                                                    PreyInCaptureRangeMutant++;
                                                }

                                                if (!IsHandling[i])
                                                {

                                                    PreyInCaptureRangeQuesting++;

                                                    if (Random.value < AdjustedCaptureProbability)
                                                    {
                                                        PreyConsumed++;

                                                        // Decactivation happens right away
                                                        // so no one else can eat this prey
                                                        IsActive[j] = false;

                                                        // Predator is now handling
                                                        IsHandling[i] = true;

                                                        // Predator gets an energy boost
                                                        Energy[i] += 1f;

                                                        // Reproduction may occur
                                                        // stealing index of just deactivated prey
                                                        if (Random.value < ConversionProbability)
                                                        {

                                                            IsActive[j] = true;
                                                            IsPrey[j] = false;
                                                            IsHandling[j] = false;
                                                            IsMutant[j] = IsMutant[i];

                                                            NextLocationX[j] = LocationXi;
                                                            NextLocationY[j] = LocationYi;

                                                            theta = Random.value * 2 * Mathf.PI;
                                                            NextVelocityX[j] = Mathf.Cos(theta) * PredSpeed;
                                                            NextVelocityY[j] = Mathf.Sin(theta) * PredSpeed;

                                                            Energy[j] = 1f;

                                                        }
                                                    }
                                                }
                                                else
                                                {
                                                    PreyInCaptureRangeHandling++;
                                                }
                                            }
                                        }
                                        // End predator-prey interactions


                                        // Begin predator conspecific behavioural interactions
                                        if (!IsPrey[j])
                                        {

                                            if (IsMutant[i])
                                            {
                                                if (IsMutant[j])
                                                {
                                                    LocalConspecificsMutant++;
                                                }
                                                else
                                                {
                                                    LocalHeterospecificsMutant++;
                                                }
                                            }


                                            if (IsMutant[i] == IsMutant[j])
                                            {

                                                LocalConspecifics++;
                                                if (IsHandling[i])
                                                {
                                                    LocalConspecificsHandling++;
                                                }
                                                else
                                                {
                                                    LocalConspecificsQuesting++;
                                                }


                                                bool condition1 =  CollectivePred & !DeviantMutant;
                                                bool condition2 =  CollectivePred &  DeviantMutant & !IsMutant[i];
                                                bool condition3 = !CollectivePred &  DeviantMutant &  IsMutant[i];
                                                if (condition1 | condition2 | condition3)
                                                {


                                                    // Add predator j to the group list for predator i
                                                    InteractingWithi.Add(j);


                                                    // Repulsion - first priority, if they are a nonzero distance away
                                                    if (dsq < RepulsionRadiusSquared)
                                                    {

                                                        IsRepulsed = true;

                                                        if (dsq > 0)
                                                        {
                                                            mag = Mathf.Sqrt(dsq);
                                                            RepulsionVelocityXi -= dx / mag;
                                                            RepulsionVelocityYi -= dy / mag;
                                                        }

                                                    }

                                                    // Attraction and alignment - while no repulsive behavior has been triggered 
                                                    if (!IsRepulsed)
                                                    {

                                                        // Attraction, if they are a nonzero distance away
                                                        if (dsq < AttractionRadiusSquared & dsq > 0)
                                                        {
                                                            mag = Mathf.Sqrt(dsq);
                                                            NextVelocityXi += dx / mag;
                                                            NextVelocityYi += dy / mag;
                                                        }

                                                        // Alignment
														if (dsq < AlignmentRadiusSquared) 
														{
															NextVelocityXi += VelocityX[j];
															NextVelocityYi += VelocityY[j];
														}


                                                       

                                                    }

                                                }

                                            }

                                        }
                                        // End predator conspecific behavioural interactions

                                    }
                                }
                            }
                        }
                    }
                    // End predator interactions with other individuals


                    // If repulsion occured, use repulsion velocity for next velocity, 
                    if (IsRepulsed)
                    {
                        NextVelocityXi = RepulsionVelocityXi;
                        NextVelocityYi = RepulsionVelocityYi;
                    }


                    // Convert next velocity to angle (the 'desired direction'), add noise
                    float DesiredDirection = Mathf.Atan2(NextVelocityYi, NextVelocityXi);
                    DesiredDirection += Rnormal() * PredNoise * TimeStep;


                    // Temper desired direction by max turn angle
                    float Direction = Mathf.Atan2(VelocityYi, VelocityXi);
                    float NextDirection = Mathf.Deg2Rad * Mathf.MoveTowardsAngle(Direction * Mathf.Rad2Deg,
                                                                                    DesiredDirection * Mathf.Rad2Deg,
                                                                                    MaxTurnAngle * Mathf.Rad2Deg);

                    // Convert back to velocity
                    NextVelocityXi = Mathf.Cos(NextDirection) * PredSpeed;
                    NextVelocityYi = Mathf.Sin(NextDirection) * PredSpeed;


                }
                // End predator updating


                // Caclulate new position
                float DisplacementXi = VelocityXi * TimeStep;
                float DisplacementYi = VelocityYi * TimeStep;
                NextLocationXi = LocationXi + DisplacementXi;
                NextLocationYi = LocationYi + DisplacementYi;


                // Boundary conditions
                // If individuals cross a boundary they appear on the opposite side just inside the boundary
                // If individuals were to ever sit exactly on the boundary they could be put in a hashing bucket outside the arena
                // leading to a 'key not found' error when updating hash
                if (NextLocationXi >= ArenaLength) { NextLocationXi = NextLocationXi - ArenaLength + 0.01f; }
                if (NextLocationXi <= 0f) { NextLocationXi = NextLocationXi + ArenaLength - 0.01f; }
                if (NextLocationYi >= ArenaLength) { NextLocationYi = NextLocationYi - ArenaLength + 0.01f; }
                if (NextLocationYi <= 0f) { NextLocationYi = NextLocationYi + ArenaLength - 0.01f; }



                // Write this individual's next location, velocity back to arrays
                NextLocationX[i] = NextLocationXi;
                NextLocationY[i] = NextLocationYi;
                NextVelocityX[i] = NextVelocityXi;
                NextVelocityY[i] = NextVelocityYi;


                // Enter predator conspecific neighbours in dictionary
                if (!IsPrey[i])
                {
                    NeighbourDict.Add(i, InteractingWithi);
                }

            }

        }


        // Calculate global polarization order and crowding ratio
        if (PredPopulationSize > 0)
        {

            // Polarization
            SumVelocity = new Vector2(SumVelocityX, SumVelocityY);
            Polarization = 1 / (PredSpeed * PredPopulationSize) * SumVelocity.magnitude;


            // Crowding
            Crowding = LocalConspecifics / PredPopulationSize;
            CrowdingHandling = LocalConspecificsHandling / PredHandling;
            CrowdingQuesting = LocalConspecificsQuesting / (PredPopulationSize - PredHandling);




        }
        else
        {

            Polarization = 0f;

        }


        // Step
        for (int i = 0; i < MaxCommunitySize; i++)
        {

            if (IsActive[i])
            {

                LocationX[i] = NextLocationX[i];
                LocationY[i] = NextLocationY[i];
                VelocityX[i] = NextVelocityX[i];
                VelocityY[i] = NextVelocityY[i];

            }

        }

        SimulationTime += TimeStep;

    }






    // Initialize the model
    public void Initialize()
    {

        RunID = Random.value;

        // Initialize step and time counters
        SimulationStep = 0;
        SimulationTime = 0;

        // Initialize location and velocity and energy
        LocationX = new float[MaxCommunitySize];
        LocationY = new float[MaxCommunitySize];
        VelocityX = new float[MaxCommunitySize];
        VelocityY = new float[MaxCommunitySize];
        NextLocationX = new float[MaxCommunitySize];
        NextLocationY = new float[MaxCommunitySize];
        NextVelocityX = new float[MaxCommunitySize];
        NextVelocityY = new float[MaxCommunitySize];
        Energy = new float[MaxCommunitySize];

        // Initialize locations, velocities and states
        IsPrey = new bool[MaxCommunitySize];
        IsActive = new bool[MaxCommunitySize];
        IsHandling = new bool[MaxCommunitySize];
        IsMutant = new bool[MaxCommunitySize];

        int InitialPopulationSize = InitialPredPopulationSize + InitialPreyPopulationSize;
        int CurrentNumberOfMutants = 0;

        for (int i = 0; i < MaxCommunitySize; i++)
        {

            if (i < InitialPopulationSize)
            {

                IsActive[i] = true;

                LocationX[i] = Random.value * ArenaLength;
                LocationY[i] = Random.value * ArenaLength;

            }


            if (i < InitialPreyPopulationSize)
            {

                IsPrey[i] = true;
                Energy[i] = 0f;

                theta = Random.value * 2 * Mathf.PI;
                VelocityX[i] = Mathf.Cos(theta) * PreySpeed;
                VelocityY[i] = Mathf.Sin(theta) * PreySpeed;

            }


            if (i >= InitialPreyPopulationSize)
            {

                if (CurrentNumberOfMutants < InitialMutantPopulationSize)
                {

                    IsMutant[i] = true;
                    CurrentNumberOfMutants++;

                }


                IsPrey[i] = false;
                Energy[i] = 1f;

                theta = Random.value * 2 * Mathf.PI;
                VelocityX[i] = Mathf.Cos(theta) * PredSpeed;
                VelocityY[i] = Mathf.Sin(theta) * PredSpeed;
            }


            IsHandling[i] = false;


        }


        // Initialize spatial hashing, preallocate bucket and neighbor dictionary with keys
        Bucket = new int[MaxCommunitySize];
        BucketLength = ArenaLength / ArenaLengthInBuckets;
        nBucket = ArenaLengthInBuckets * ArenaLengthInBuckets;
        BucketConversionFactor = 1f / BucketLength;

        BucketDict = new Dictionary<int, List<int>>(nBucket);
        InitializeBucketDict();


        // Initialize group tracking
        NeighbourDict = new Dictionary<int, List<int>>();
        GroupDict = new Dictionary<int, List<int>>();
        InFocalGroup = new bool[MaxCommunitySize];


    }






    // Approximate a sample from a standard normal distribution
    // using the sum of 12 random draws from uniform(0,1) which 
    // is approximately normal with a mean of 6 and s.d. of 1
    float Rnormal()
    {

        float x = 0f;
        for (int i = 0; i < 12; i++)
        {
            x += Random.value;
        }
        return (x - 6);

    }




    // Find inactive index for birth events
    int FindInactiveIndex()
    {
        int k = 0;
        while (true)
        {

            if (!IsActive[k])
            {
                break;
            }

            k++;

            if (k == MaxCommunitySize)
            {
                k = -1;                                     // Error code: max community size reached
                break;
            }
        }

        return (k);
    }






    // Initialize spatial hash
    void InitializeBucketDict()
    {

        BucketDict.Clear();

        for (int i = 0; i < nBucket; i++)
        {
            List<int> val = new List<int>();
            BucketDict.Add(i, val);
        }

    }





    // Update spatial hash
    void UpdateBucketDict()
    {

        int key;
        int x;
        int y;

        InitializeBucketDict();

        for (int i = 0; i < MaxCommunitySize; i++)
        {

            if (IsActive[i])
            {

                x = (int)(LocationX[i] * BucketConversionFactor);
                y = (int)(LocationY[i] * BucketConversionFactor);

                key = x + y * ArenaLengthInBuckets;

                List<int> val = new List<int>();

                val = BucketDict[key];
                val.Add(i);

                BucketDict[key] = val;
                Bucket[i] = key;

            }

        }

    }


    // Track groups using method for equivalence classes 
    // described in Press et al. Numerical Recipes Chapter 8.6
    public void TrackGroups()
    {
        bool IsEquiv;
        List<int> Neighbours = new List<int>();

        int n = NeighbourDict.Count;

        if (n > 0)
        {

            int[] GroupIndex = new int[n];
            int[] IdvlIndex = new int[n];

            NeighbourDict.Keys.CopyTo(IdvlIndex, 0);


            // begin method from Press et al.
            GroupIndex[0] = 0;
            for (int j = 1; j < n; j++)
            {

                GroupIndex[j] = j;

                Neighbours.Clear();
                Neighbours = NeighbourDict[IdvlIndex[j]];

                for (int k = 0; k < j; k++)
                {

                    GroupIndex[k] = GroupIndex[GroupIndex[k]];

                    IsEquiv = Neighbours.Contains(IdvlIndex[k]);
                    if (IsEquiv)
                    {
                        GroupIndex[GroupIndex[GroupIndex[k]]] = j;
                    }

                }

            }

            for (int j = 0; j < n; j++)
            {
                GroupIndex[j] = GroupIndex[GroupIndex[j]];
            }
            // end method from Press et al.


            // Write to group dictionary
            for (int j = 0; j < n; j++)
            {

                int group = GroupIndex[j];
                List<int> members = new List<int>();

                if (GroupDict.ContainsKey(group))
                {
                    members = GroupDict[group];
                    members.Add(IdvlIndex[j]);
                    GroupDict[group] = members;
                }
                else
                {
                    members.Add(IdvlIndex[j]);
                    GroupDict.Add(group, members);
                }

            }


            // Number of groups and statistics on group size
            int LargestGroup = 0;
            int SumGroupSize = 0;
            int SumGroupSizeSquared = 0;
   

            // initialize largest group size and index from first dict entry
            foreach (int group in GroupDict.Keys)
            {
                MaxGroupSize = GroupDict[group].Count;
                LargestGroup = group;
                break;
            }


            // then scan through to update to correct value for max group size
            // and also count singleton groups
            // and also count number of mutant groups
            foreach (int group in GroupDict.Keys)
            {

                bool isMutantGroup = false;
                List<int> members = new List<int>();
                members = GroupDict[group];

                int thisgroupsize = members.Count;

                SumGroupSize += thisgroupsize;
                SumGroupSizeSquared += thisgroupsize * thisgroupsize;

                if (thisgroupsize > MaxGroupSize)
                {
                    MaxGroupSize = thisgroupsize;
                    LargestGroup = group;
                }

                if (thisgroupsize == 1)
                {
                    NumberOfSingletonGroups++;
                }


                foreach (int idvl in members)
                {
                    isMutantGroup = IsMutant[idvl];
                    break;
                }

                if (isMutantGroup)
                {
                    NumberOfMutantGroups++;
                }


            }


            // For visualization, who is in focal (largest) group 
            List<int> FocalIndividuals = GroupDict[LargestGroup];
            foreach (int j in FocalIndividuals)
            {
                InFocalGroup[j] = true;
            }



            // Calculate remaining public variables (MaxGroupSize and NumberOfSingletonGroups done)
            NumberOfGroups = GroupDict.Keys.Count;
            MeanGroupSize = (float)SumGroupSize / NumberOfGroups;
            VarGroupSize = (float)SumGroupSizeSquared / NumberOfGroups - (MeanGroupSize * MeanGroupSize);

        }

    }

}