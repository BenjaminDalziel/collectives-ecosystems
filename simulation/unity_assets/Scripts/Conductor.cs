using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.IO;

public class Conductor : MonoBehaviour {

	public int StepsPerFrame;
	public int MaxSteps;
	public bool RecordData;
    public int StepsPerRecord;
    public int Replicates;
    public int EnrichmentIncrement;                              // Amount by which prey carrying capacity is incremented each replicate
    public bool HalftimeSwitch;                                // Toggle collective behavior halfway through simulation   
    public bool GroupTracking;


    private ModelBehavior model;
    private ParameterRecorder pararecorder;
    private DataRecorder datarecorder;
	private int StepsTaken;
    private int StepsTakenSinceLastRecord;
	private bool KeepGoing;
    private int ReplicatesCompleted;
    private bool HalftimeSwitchIsComplete;
    private int HalfMaxSteps;


    void Awake () {

		model = GetComponent<ModelBehavior> ();

        if (RecordData) {

           if (!Directory.Exists(model.OutputPath))
                System.IO.Directory.CreateDirectory(model.OutputPath);

            pararecorder = GetComponent<ParameterRecorder>();
            datarecorder = GetComponent<DataRecorder>();

		}

        ReplicatesCompleted = 0;
        HalfMaxSteps = MaxSteps / 2;

        SetupSimulation();
    }





    void SetupSimulation () {

        model.Initialize();
        CheckParameters();

        if (HalftimeSwitch & HalftimeSwitchIsComplete) {
            model.CollectivePred = !model.CollectivePred;
            HalftimeSwitchIsComplete = false;
        }

        KeepGoing = true;
        StepsTaken = 0;
        StepsTakenSinceLastRecord = 0;
    
        if (RecordData) {

            pararecorder.Initialize();
            pararecorder.SaveNow();
            pararecorder.CloseNow();

            datarecorder.Initialize();

         }

     }




	void Update () 
    {


        if (KeepGoing) {


            // Announce start of simulation, if it is the start
            if( StepsTaken == 0){
                Debug.Log("Starting next replicate.");
            }



			// Step forward the specified number of steps for one frame
			for (int i=0; i<StepsPerFrame; i++) 
			{
				model.Step ();
				StepsTaken++;
                StepsTakenSinceLastRecord++;
			}


            // Do group tracking, if enabled
            if (GroupTracking)
            {
                model.TrackGroups();
            }



            // Possibly save state
            if (RecordData) {
                if (StepsTakenSinceLastRecord >= StepsPerRecord) {

                    datarecorder.SaveNow();
                    StepsTakenSinceLastRecord = 0;

                }
            }



            // Check for halftime switch
            if (HalftimeSwitch)
            {
                if (!HalftimeSwitchIsComplete)
                {

                    if (StepsTaken > HalfMaxSteps)
                    {

                        model.CollectivePred = !model.CollectivePred;
                        HalftimeSwitchIsComplete = true;
                        Debug.Log("Halftime switch complete.");

                    }
                }
            }


            // Check for doneness and respond accordingly
            bool IsCompletedRep = StepsTaken > MaxSteps | model.PredPopulationSize == 0;

            if (IsCompletedRep)
            {

                if (RecordData)
                {
                    datarecorder.CloseNow();
                }

                KeepGoing = false;
                ReplicatesCompleted++;
                Debug.Log("Replicate complete.");

                if (ReplicatesCompleted < Replicates)
                {

                    model.CarryingCapacity += EnrichmentIncrement;
                    SetupSimulation();

                }
                else
                {

                    Debug.Log("All replicates complete.");

                }
            }


        }
    }







    void CheckParameters ()
    {

        bool IsBucketLengthTooSmall = model.BucketLength < model.InteractionRadius;
        bool IsInteractionRadiusTooSmall = model.InteractionRadius < model.CaptureRadius | model.InteractionRadius < model.RepulsionRadius;

        if (IsBucketLengthTooSmall)
            Debug.Log("Warning: Interaction radius exceeds bucket length. Some positive interaction probabilities are being effectively set to 0.");

        if (IsInteractionRadiusTooSmall)
            Debug.Log("Warning: Interaction radius is not as large as one or more other radii. Some positive interaction probabilities are being effectively set to 0.");


    }




}
