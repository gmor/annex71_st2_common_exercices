from fmpy import read_model_description, extract
from fmpy import *
from fmpy.fmi2 import FMU2Slave
from fmpy.util import plot_result, download_test_file
import numpy as np
import shutil
import os


# pulse secure:
# r0753014
# MPCsubex1

def simulate_with_input():
    # os.chdir(r"C:\arash\PHD\IEA Projects\Annex\common exercise\ajab")
    os.chdir(path="C:/Users/gerar/PycharmProjects/untitled1/venv/Scripts")
    fmuf = 'TwinHouses71_new.fmu'
    start_time = 30463200
    # stop_time = 31676400
    # step_size = 3600
    #dump(fmuf)
    model_description = read_model_description(fmuf)
    vrs = {}
    for variable in model_description.modelVariables:
        vrs[variable.name] = variable.valueReference
        # print(variable)

    vr_output1 = vrs['Tav']  # temperature
    vr_output2 = vrs['hp_el'] # heat pump consumption
    vr_input1 = vrs['hp_s']  # heat pump status
    vr_input2 = vrs['hp_wt']  # heat water temperature
    #vr_outputs2 =vrs['lagtemp'] #hourly lagged temperature
    unzipdir = extract(fmuf)
    fmu = FMU2Slave(guid=model_description.guid, modelIdentifier=model_description.coSimulation.modelIdentifier,
                    unzipDirectory=unzipdir, instanceName='instance1')
    fmu.instantiate()
    fmu.setupExperiment(startTime=start_time)
    fmu.enterInitializationMode()
    fmu.exitInitializationMode()
    # fmu.callbacks
    # output1=20

    return fmu, vr_input1, vr_input2, vr_output1, vr_output2

# fmu, vr_input1, vr_input2, vr_output1, vr_output2 = simulate_with_input()

def Iteration(fmu, vr_input1, vr_input2, vr_output1, vr_output2, time, step_size, Tsup, HPs):
    # step_size = 3600
    fmu.setReal([vr_input1], [HPs]) #k
    fmu.setReal([vr_input2], [Tsup]) #z

    fmu.doStep(currentCommunicationPoint=time, communicationStepSize=step_size)
    # inputs, outputs4 = fmu.getReal([vr_inputs, vr_outputs4])

    input1, input2, output1, output2 = fmu.getReal([vr_input1, vr_input2, vr_output1, vr_output2])

    return fmu, input1, input2, output1, output2

def END_fmu(fmu):
    unzipdir = extract(fmuf)
    fmu.terminate()
    fmu.freeInstance()

    shutil.rmtree(unzipdir)

    return

    # result = np.array(rows, dtype=np.dtype([('time', np.float64), ('inputs', np.float64), ('outputs[0]', np.float64)]))
