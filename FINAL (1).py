import random
import numpy as np
from queue import Queue


def main():
    mri = MRI()
    day_type1 = 1
    day_type2 = 1
    day = 1
    machine = 1
    old_schedule = old_schedule_computation(mri, day_type1, day_type2)
    new_schedule = new_schedule_computation(mri, machine, day)
    total_idle_time_type1 = 0
    total_idle_time_type2 = 0
    total_idle_time_type3 = 0
    total_idle_time_type4 = 0

    for current_time, patient_id, day, p_type in old_schedule:
        mri.old_simulation(patient_id, current_time, day, p_type)
    # print_old_simulation(mri.schedule_type1, 1)  This prints the schedule for machine 1 (old)
    average_waiting_time, average_scan_time, average_time_system, max_waiting_time, total_idle_time = statistics(
        mri.schedule_type1, total_idle_time_type1)
    print(f"\nStatistics for machine 1 (Patient Type 1):")
    print(f"Average Waiting Time in the Queue: {average_waiting_time:.2f} minutes")
    print(f"Average Scan Time: {average_scan_time:.2f} minutes")
    print(f"Average Time in the System: {average_time_system:.2f} minutes")
    print(f"Maximum Waiting Time: {max_waiting_time} minutes")
    print(f"Total Idle Time: {total_idle_time} minutes")
    print("\n")
    print("\n")
    # print_old_simulation(mri.schedule_type2, 2)    This prints the schedule for machine 2 (old)
    average_waiting_time, average_scan_time, average_time_system, max_waiting_time, total_idle_time = statistics(
        mri.schedule_type2, total_idle_time_type2)
    print(f"\nStatistics for machine 2 (Patient Type 2):")
    print(f"Average Waiting Time in the Queue: {average_waiting_time:.2f} minutes")
    print(f"Average Scan Time: {average_scan_time:.2f} minutes")
    print(f"Average Time in the System: {average_time_system:.2f} minutes")
    print(f"Maximum Waiting Time: {max_waiting_time} minutes")
    print(f"Total Idle Time: {total_idle_time} minutes")
    print("\n")
    print("\n")
    
    for current_time, patient_id, day, p_type, machine in new_schedule:
        mri.new_simulation(patient_id, current_time, day, p_type, machine)
    # print_new_simulation(mri.schedule_type3, 3,1)  This prints the schedule for machine 1 (new)
    average_waiting_time, average_scan_time, average_time_system, max_waiting_time, total_idle_time = statistics(
        mri.schedule_type3, total_idle_time_type3)
    print(f"\nStatistics for machine 1 (Both patient types):")
    print(f"Average Waiting Time in the Queue: {average_waiting_time:.2f} minutes")
    print(f"Average Scan Time: {average_scan_time:.2f} minutes")
    print(f"Average Time in the System: {average_time_system:.2f} minutes")
    print(f"Maximum Waiting Time: {max_waiting_time} minutes")
    print(f"Total Idle Time: {total_idle_time} minutes")
    print("\n")
    print("\n")
    # print_new_simulation(mri.schedule_type4, 3,2)  This prints the schedule for machine 2 (new)
    average_waiting_time, average_scan_time, average_time_system, max_waiting_time, total_idle_time = statistics(
        mri.schedule_type4, total_idle_time_type4)
    print(f"\nStatistics for machine 2 (Both patient types):")
    print(f"Average Waiting Time in the Queue: {average_waiting_time:.2f} minutes")
    print(f"Average Scan Time: {average_scan_time:.2f} minutes")
    print(f"Average Time in the System: {average_time_system:.2f} minutes")
    print(f"Maximum Waiting Time: {max_waiting_time} minutes")
    print(f"Total Idle Time: {total_idle_time} minutes")


class MRI:
    def __init__(self):
        self.schedule_type1 = []
        self.schedule_type2 = []
        self.schedule_type3 = []
        self.schedule_type4 = []
        
    def new_simulation(self, patient_id, current_time, day, patient_type, machine):
        schedule = []
        if machine == 1:
            schedule = self.schedule_type3
        elif machine == 2:
            schedule = self.schedule_type4
        if patient_type == 1:
            scan_duration = max(0, round(np.random.normal(25.954, 5.858205)))    # SCAN DURATION PATIENT T1
        elif patient_type == 2:
            scan_duration = max(0, round(np.random.gamma(shape=769.956, scale=0.052143)))  # SCAN DURATION PATIENT T1
        if not schedule:
            start = current_time 
        else:
            last_scan = schedule[-1]
            end_of_last_scan = last_scan[2] + last_scan[3]
            start = max(current_time, end_of_last_scan)
            if current_time > 540 * day:
                start = 540 * (day - 1)
        scan_time = start
        schedule.append((patient_id, current_time,  scan_time, scan_duration, machine, day))
        schedule[-1] = schedule[-1][:3] + (scan_duration, day, machine)

    def old_simulation(self, patient_id, current_time, day, patient_type):
        if patient_type == 1:
            schedule = self.schedule_type1
            scan_duration = max(0, round(np.random.normal(25.954, 5.858205)))  # SCAN DURATION FOR PATIENT T1
        elif patient_type == 2:
            schedule = self.schedule_type2
            scan_duration = max(0, round(np.random.gamma(shape=769.956, scale=0.052143)))  # SCAN DURATION PATIENT T1
        if not schedule:
            start = current_time
        else:
            last_appointment = schedule[-1]
            end_of_last_appointment = last_appointment[2] + last_appointment[3]
            start = max(current_time, end_of_last_appointment)
            if current_time > 540 * day:
                start = 540 * (day - 1)

        appointment_time = start
        schedule.append((patient_id, current_time,  appointment_time, scan_duration, day))
        schedule[-1] = schedule[-1][:3] + (scan_duration, day)


def new_schedule_computation(mri, day, machine_type):
    patient_id_machine1 = 0
    patient_id_machine2 = 0
    current_time_machine1 = 0
    current_time_machine2 = 0
    current_time = 0
    queue = Queue()
    patient_id_type1 = 0
    patient_id_type2 = 0
    day = 1
    interval_type1 = max(1, np.random.exponential(scale=32.62168))    # NEW SYSTEM INTERVAL FOR PATIENT T1
    interval_type2 = max(1, round(np.random.weibull(3.021331) * 57.94103))    # NEW SYSTEM INTERVAL FOR PATIENT T2
    real_time_1 = 0
    real_time_2 = 0
    timeslot_1 = 25                     # Choose TIMESLOT PATIENT TYPE 1
    timeslot_2 = 40                     # Choose TIMESLOT PATIENT TYPE 2

    while current_time < 540:
        if current_time % interval_type1 == 0:
            queue.put(1)
        if current_time % interval_type2 == 0:
            queue.put(2)
        current_time += 1
    
    while day <= 4:                                                # FILL IN THE NUMBER OF DAYS
        if current_time_machine1 == (day-1)*540:
            queue.put(1)
            queue.put(2)
        patient_considered = 0
        while real_time_1 >= interval_type1:
            if real_time_1 >= interval_type1:
                queue.put(1)
                real_time_1 = real_time_1-interval_type1
            else:
                break
        while real_time_2 >= interval_type2:
            if real_time_2 >= interval_type2:
                queue.put(2)
                real_time_2 = real_time_2 - interval_type2
            else:
                break

        if current_time_machine1 <= 540 * day and patient_considered == 0 \
                and current_time_machine1 <= current_time_machine2:
            if not queue.empty():
                person_considered = queue.get()
                if person_considered == 1:
                    yield current_time_machine1, patient_id_machine1, day, 1, 1
                    patient_id_machine1 += 1
                    current_time_machine1 += timeslot_1
                    real_time_1 += timeslot_1
                    real_time_2 += timeslot_1
                    patient_id_type1 += 1
                    patient_considered = 1
                elif person_considered == 2:
                    yield current_time_machine1, patient_id_machine1, day, 2, 1
                    patient_id_machine1 += 1
                    current_time_machine1 += timeslot_2
                    patient_id_type2 += 1
                    patient_considered = 1
                    real_time_1 += timeslot_2
                    real_time_2 += timeslot_2
                else:
                    break
                 
        elif current_time_machine2 <= 540 * day and patient_considered == 0 \
                and current_time_machine2 < current_time_machine1:
            if not queue.empty():
                person_considered = queue.get()
                if person_considered == 1:
                    yield current_time_machine2, patient_id_machine2, day, 1, 2
                    patient_id_machine2 += 1
                    current_time_machine2 += timeslot_1
                    patient_id_type1 += 1
                    patient_considered = 1
                    real_time_1 += timeslot_1
                    real_time_2 += timeslot_1
                elif person_considered == 2:
                    yield current_time_machine2, patient_id_machine2, day, 2, 2
                    patient_id_machine2 += 1
                    current_time_machine2 += timeslot_2
                    patient_id_type2 += 1
                    patient_considered = 1
                    real_time_1 += timeslot_2
                    real_time_2 += timeslot_2
            else:
                break
        else:
            yield current_time_machine2, patient_id_machine2, day, 2, 1
            yield current_time_machine2, patient_id_machine2, day, 2, 2
            day += 1
            current_time_machine1 = (day-1)*540
            current_time_machine2 = (day-1)*540


def old_schedule_computation(mri,day_type1,day_type2):
    patient_id_type1 = 0
    patient_id_type2 = 0
    current_time_type1 = 0
    current_time_type2 = 0
    day_type1 = 1
    day_type2 = 1
    patient_number_type1 = 1
    patient_number_type2 = 1
    extra_patients_type1 = 0
    extra_patients_type2 = 0
    timeslot_1 = 25                     # Choose TIMESLOT PATIENT TYPE 1
    timeslot_2 = 40                     # Choose TIMESLOT PATIENT TYPE 2
    while day_type1 < 5: 
        if day_type1 < 5:                                 # FILL IN THE NUBER OF DAYS
            number_of_patients_type1_gen = \
                max(1, round(540/(np.random.exponential(scale=32.62168))))+extra_patients_type1  # THIS IS NOT THE INTERVAL BUT THE TOTAL NUMBER OF PATIENTS PER DAY
        else:
            number_of_patients_type1_gen = extra_patients_type1
            if number_of_patients_type1_gen == 1 or number_of_patients_type1_gen == 0:
                break
        while patient_number_type1 <= number_of_patients_type1_gen:
            if current_time_type1 <= 540 * day_type1 and patient_number_type1 != number_of_patients_type1_gen:
                yield current_time_type1, patient_id_type1, day_type1, 1
                patient_id_type1 += 1
                patient_number_type1 += 1
                current_time_type1 += timeslot_1
            else:
                extra_patients_type1 = number_of_patients_type1_gen - patient_number_type1
                yield current_time_type1, patient_id_type1, day_type1, 1
                patient_id_type1 += 1
                day_type1 += 1
                current_time_type1 = (day_type1 - 1) * 540
                patient_number_type1 = 1
                break
      
    while day_type2 < 5: 
        if day_type2 < 5:                                                  #FILL IN THE NUMBER OF DAYS
            number_of_patients_type2_gen = \
                max(1,round(540/(np.random.weibull(3.021331) * 57.94103))) + extra_patients_type2   # THIS IS NOT THE INTERVAL BUT THE TOTAL NUMBER OF PATIENTS PER DAY
        else:
            number_of_patients_type2_gen = extra_patients_type2
            if number_of_patients_type2_gen == 1 or number_of_patients_type2_gen == 0:
                break
        while patient_number_type2 <= number_of_patients_type2_gen:
            if current_time_type2 <= 540*day_type2 and patient_number_type2 != number_of_patients_type2_gen:
                yield current_time_type2, patient_id_type2, day_type2, 2
                patient_id_type2 += 1
                patient_number_type2 += 1
                current_time_type2 += timeslot_2
            else:
                extra_patients_type2 = number_of_patients_type2_gen-patient_number_type2
                yield current_time_type2, patient_id_type2, day_type2, 2
                patient_id_type2 += 1
                day_type2 += 1
                current_time_type2 = (day_type2 - 1) * 540
                patient_number_type2 = 1
                break


def print_old_simulation(schedule, patient_type):
    print(f"Schedule for Machine {patient_type}:")
    number_of_patients = 0
    for appointment in schedule:
        patient_id = appointment[0]
        day = appointment[4]
        arrival_time = appointment[1]
        queue_waiting_time = appointment[2] - arrival_time
        scan_start_time = appointment[2]
        scan_duration = appointment[3]
        if queue_waiting_time >= 0:
            hours_arrival = 8 + round((arrival_time-(day-1)*540)) //60
            minutes_arrival = round((arrival_time-(day-1)*540)) % 60
            time_arrival = f"{hours_arrival:02d}:{minutes_arrival:02d}"
            hours_start = 8 + round((scan_start_time-(day-1)*540)) //60
            minutes_start = round((scan_start_time-(day-1)*540)) % 60
            time_start = f"{hours_start:02d}:{minutes_start:02d}"
            print(f"On day {day}")            
            # print(patient_id+1, end='')
            print(f"Patient (Type {patient_type}) arrives at ", end='')
            print(time_arrival, end='')
            # print(arrival_time, end='')
            print(f", waits in the queue for {queue_waiting_time} minutes, enters the scan at ", end='')
            print(time_start, end='')
            # print(scan_start_time, end='')
            print(f". The scan takes {scan_duration} minutes.\n")
            number_of_patients+=1
    print(f"There are ", end="")
    print(number_of_patients, end="")
    print(f" patients scanned.")


def print_new_simulation(schedule, patient_type, machine):
    print(f"Schedule for Machine {machine} (for both patient types):")
    number_of_patients = 0
    for appointment in schedule:
        day = appointment[4]
        arrival_time = appointment[1]
        queue_waiting_time = appointment[2] - arrival_time
        scan_start_time = appointment[2]
        scan_duration = appointment[3]
        machine = appointment[5]
        if queue_waiting_time >= 0:
            hours_arrival = 8 + round((arrival_time-(day-1)*540)) // 60
            minutes_arrival = round((arrival_time-(day-1)*540)) % 60
            time_arrival = f"{hours_arrival:02d}:{minutes_arrival:02d}"
            hours_start = 8 + round((scan_start_time-(day-1)*540)) // 60
            minutes_start = round((scan_start_time-(day-1)*540)) % 60
            time_start = f"{hours_start:02d}:{minutes_start:02d}"
            print(f"On day {day}")            
            # print(patient_id+1, end='')
            print(f"Patient arrives at ", end='')
            print(time_arrival, end='')
            # print(arrival_time, end='')
            print(f", waits in the queue for {queue_waiting_time} minutes, enters the scan at ", end='')
            print(time_start, end='')
            # print(scan_start_time, end='')
            print(f". The scan takes {scan_duration} minutes.\n")
            number_of_patients+=1
    print(f"There are ", end="")
    print(number_of_patients,end="")
    print(f" patients scanned.") 


def statistics(schedule, total_idle_time):
    total_waiting_time = 0
    total_scan_time = 0
    total_time_system = 0
    max_waiting_time = 0
    previous_appointment_end = 0
    for statistics in schedule:
        queue_duration = statistics[2] - statistics[1]
        time_system = statistics[3]
        if queue_duration >= 0 and time_system >= 0:
            total_waiting_time += queue_duration
            total_scan_time += time_system
            total_time_system += time_system + queue_duration
            max_waiting_time = max(max_waiting_time, queue_duration)
            
            idle_time = statistics[2] - previous_appointment_end
            if idle_time > 0:
                total_idle_time += idle_time
                previous_appointment_end = statistics[2] + time_system

    average_waiting_time = total_waiting_time / len(schedule)
    average_scan_time = total_scan_time / len(schedule)
    average_time_system = total_time_system / len(schedule)

    return average_waiting_time, average_scan_time, average_time_system, max_waiting_time, total_idle_time


if __name__ == "__main__":
    random.seed(12)
    np.random.seed(12)
    main()
