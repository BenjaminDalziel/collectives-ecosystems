using System.Collections.Generic;
using UnityEngine;

public class MeshController : MonoBehaviour {

	public float BodySize;

    public bool ShowHandling;

    public bool EnergyGoggles;				   
    public bool NeighborGoggles;
    public bool GroupGoggles;

    public float EnergyClip;                        // the value at which energy color scheme reaches maximum brightness

    private float BodyWidth;
    private float BodyLengthPrey;
    private float BodyLengthPred;
    private ModelBehavior model;
	private Mesh mesh;
	private Vector3[] vertices;
	private int[] triangles;
	private Vector3[] normals;
	private Vector2[] uv;
	private Texture2D texture;
	private int meshSize;
	private float[] energy;
	private float alpha;							// generic value for setting UV
    private int focalPred;

	void Start ()
	{

		model = GetComponent<ModelBehavior> ();
		GenerateTexture ();
		GenerateMesh ();

	}


	void Update ()
	{

		UpdateUV ();
		UpdateVertices ();

	}


	void GenerateTexture (){

		int res = 2;
		texture = new Texture2D (res,res,TextureFormat.RGBA32,false);

		Color prey = new Color (0.3f, 0.4f, 0.5f, 1f);     
		Color pred = new Color (1.0f, 0.75f, 0.22f, 1f);      
		Color lite = new Color (1f, 1f, 1f, 1f);
		Color dark = new Color (0f, 0f, 0f, 1f);

		texture.SetPixel (0,0,prey);
		texture.SetPixel (1,1,pred);
		texture.SetPixel (0,1,dark);
		texture.SetPixel (1,0,lite);

        texture.Apply ();
        GetComponent<Renderer>().material.mainTexture = texture;
		GetComponent<Renderer>().material.mainTexture.wrapMode = TextureWrapMode.Clamp;

	}


	void GenerateMesh ()
	{

        // Get size from model
        meshSize = model.MaxCommunitySize;


        //Get mesh
        mesh = GetComponent<MeshFilter>().mesh;
		mesh.Clear ();


		//Assign vertices (varies with simulation state)
		vertices = new Vector3[3*meshSize];
		UpdateVertices ();


		//Assign uv (varies with simulation state)
		uv = new Vector2[3*meshSize];
		UpdateUV ();


		//Assign triangles (does not vary with simulation state)
		triangles = new int[6*meshSize];
		int j=0;
		int k=0;
		while ( j < 6*meshSize ){

			triangles[j] = k;
			triangles[j+1] = k+1;
			triangles[j+2] = k+2;

			triangles[j+3] = k;
			triangles[j+4] = k+2;
			triangles[j+5] = k+1;

			j+=6;
			k+=3;
		}
		mesh.triangles = triangles;


		//Assign normals (does not vary with simulation state)
		normals = new Vector3[3*meshSize];
		for (int i=0; i<3*meshSize; i++)
			normals[i] = -Vector3.forward;
		mesh.normals = normals;

	}


	void UpdateVertices ()
	{

        BodyWidth = BodySize / 3;
        BodyLengthPrey = BodySize / model.PreySpeed;
        BodyLengthPred = BodySize / model.PredSpeed;

        float[] positionX = model.LocationX;
		float[] positionY = model.LocationY;

		float[] velocityX = model.VelocityX;
		float[] velocityY = model.VelocityY;

		bool[] isActive = model.IsActive;

		int j = 1;
		for (int i=0; i<meshSize; i++){

			// Vertex locations for non-active individuals
			float xi = -99f;
			float yi = -99f;

			float vxi = 0f;
			float vyi = 0f;

			float wxi = 0f;
			float wyi = 0f;

			// Vertex locations for active indiviudals
			if ( isActive[i] ) {

				xi = positionX[i];
				yi = positionY[i];

                if(model.IsPrey[i]){
                    vxi = velocityX[i] * BodyLengthPrey;
                    vyi = velocityY[i] * BodyLengthPrey;
                }else{
                    vxi = velocityX[i] * BodyLengthPred;
                    vyi = velocityY[i] * BodyLengthPred;
                }
				

				float phi = Mathf.Atan2(vyi,vxi);
				float theta = phi + Mathf.PI/2;

				wxi = Mathf.Cos(theta)*BodyWidth;
				wyi = Mathf.Sin(theta)*BodyWidth;

			}

			vertices[j-1] = new Vector3(xi + vxi, yi + vyi);
			vertices[ j ] = new Vector3(xi + wxi, yi + wyi);
			vertices[j+1] = new Vector3(xi - wxi, yi - wyi);

			j+=3;

		}

		mesh.vertices = vertices;
		mesh.RecalculateBounds();

	}


    void UpdateUV()
    {


        focalPred = model.FocalPred;


        // GroupGoggles colorscheme
        if (GroupGoggles)
        {
        
            int j = 0;
            for (int i = 0; i < meshSize; i++)
            {

                j = 3 * i;

                alpha = model.InFocalGroup[i] ? 1f : 0.45f;

                if (!model.IsPrey[i])
                {

                    uv[ j ] = new Vector2(alpha, 1f);
                    uv[j+1] = new Vector2(alpha, 1f);
                    uv[j+2] = new Vector2(alpha, 1f);

                }
                else
                {

                    uv[ j ] = new Vector2(0f, 1-alpha);
                    uv[j+1] = new Vector2(0f, 1-alpha);
                    uv[j+2] = new Vector2(0f, 1-alpha);

                }




            }

            mesh.uv = uv;

        }


        // EnergyGoggles colorscheme
        if (EnergyGoggles) {									

			energy = model.Energy;

            int j;
			for (int i = 0; i < meshSize; i++) {

                j = 3 * i;

				alpha = Mathf.Min (energy[i], EnergyClip) / EnergyClip;
				uv[ j ] = new Vector2(alpha, 1-alpha);
				uv[j+1] = new Vector2(alpha, 1-alpha);
				uv[j+2] = new Vector2(alpha, 1-alpha);

			}

			mesh.uv = uv;

		} 
			

        // NeighborGoggles colorscheme
        if (NeighborGoggles) 
        {                                    
 
            int j;

            // Non neighbors
            for (int i = 0; i < meshSize; i++) {
                
                j = 3 * i;

                alpha = 0.2f;

                uv[ j ] = new Vector2(alpha, 1-alpha);
                uv[j+1] = new Vector2(alpha, 1-alpha);
                uv[j+2] = new Vector2(alpha, 1-alpha);

            }


            // Neighbors
            int gridx = (int)(model.LocationX[focalPred] * model.BucketConversionFactor);
            int gridy = (int)(model.LocationY[focalPred] * model.BucketConversionFactor); 
            int x;
            int y;
            int bucket;
            List<int> neighbors = new List<int>();

            for (int ix = gridx - 1; ix <= gridx + 1; ix++)
            {

                x = ix;
                if (ix == -1) { x = model.ArenaLengthInBuckets - 1; }
                if (ix == model.ArenaLengthInBuckets) { x = 0; }

                for (int iy = gridy - 1; iy <= gridy + 1; iy++)
                {

                    y = iy;
                    if (iy == -1) { y = model.ArenaLengthInBuckets - 1; }
                    if (iy == model.ArenaLengthInBuckets) { y = 0; }

                    bucket = x + y * model.ArenaLengthInBuckets;
                    neighbors = model.BucketDict[bucket];


                    alpha = 0.4f;
                    if (ix == gridx & iy == gridy)
                    {
                        alpha = 0.6f;
                    }

                    for (int i = 0; i < neighbors.Count; i++)
                    {

                        j = 3 * neighbors[i];

                        uv[j] = new Vector2(alpha, 1 - alpha);
                        uv[j + 1] = new Vector2(alpha, 1 - alpha);
                        uv[j + 2] = new Vector2(alpha, 1 - alpha);

                    }

                }

             }

            // In capture range of focal predator
            for (int i = 0; i < model.InFocalCaptureRange.Count; i++){

                j = 3 * model.InFocalCaptureRange[i];

                alpha = 0f;

                uv[j] = new Vector2(alpha, alpha);
                uv[j + 1] = new Vector2(alpha, alpha);
                uv[j + 2] = new Vector2(alpha, alpha);

            }



            // Focal predator
            j = model.FocalPred * 3;

            alpha = 1f;

            uv[j] = new Vector2(alpha, alpha);
            uv[j + 1] = new Vector2(alpha, alpha);
            uv[j + 2] = new Vector2(alpha, alpha);


            mesh.uv = uv;

        }


        // Regular colorscheme
        if (!EnergyGoggles & !NeighborGoggles & !GroupGoggles) {					
		
			bool[] isPrey = model.IsPrey;
			bool[] isHandling = model.IsHandling;
            bool[] isMutant = model.IsMutant;

			int j;
			for (int i=0; i<meshSize; i++){

                j = 3 * i;

				if( isPrey[i] ) {

					uv[ j ] = new Vector2(0f,0f);
					uv[j+1] = new Vector2(0f,0f);
					uv[j+2] = new Vector2(0f,0f);

				}

				if ( !isPrey[i] ) {
					
                    if ( isHandling [i] & ShowHandling ) {

						uv [ j ] = new Vector2 (1f,0f);
						uv [j+1] = new Vector2 (1f,0f);
						uv [j+2] = new Vector2 (1f,0f);

					} else {

                        if( isMutant[i])
                        {

                            uv[ j ] = new Vector2 (1f, 0f);
                            uv[j+1] = new Vector2 (1f, 0f);
                            uv[j+2] = new Vector2 (1f, 0f);

                        } else {

                            uv[ j ] = new Vector2 (1f, 1f);
                            uv[j+1] = new Vector2 (1f, 1f);
                            uv[j+2] = new Vector2 (1f, 1f);

                        }


					}
				}

			}

			mesh.uv = uv;

		}
	}


}